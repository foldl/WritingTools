unit UnitUnifiedLLM;

{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
  Classes, SysUtils, superobject, LibChatLLM;

type

  { TLLMFactory }

  TLLMFactory = class
  public
    class function Create(AOptions: ISuperObject): TBaseChatLLM;
  end;

implementation

uses
{$if defined(fpc)}
  fphttpclient, opensslsockets
{$elseif defined(dcc)}
  System.Net.HttpClient, System.SyncObjs, System.Threading
{$endif}
;

type
  TChatMessage = record
    Role: string;
    Content: string;
  end;

  { TOpenAILLM }

  TOpenAILLM = class(TBaseChatLLM)
  private
{$if defined(fpc)}
    FCS: TRTLCriticalSection;
    FHttp: TFPHTTPClient;
{$elseif defined(dcc)}
    FCS: TCriticalSection;
    FHttp: THTTPClient;
    FAbort: Boolean;
{$endif}
    FResponseStream: TMemoryStream;
    FReadPos: Int64;
    FAPIPath: string;
    FData: TStringList;
    FStop: string;
    FOutputAcc: string;
    FThoughtAcc: string;
    FHistory: array of TChatMessage;
    FModelName: string;
    FAIPrefix: string;
    procedure SetBusy(AValue: Boolean);
  public
    constructor Create(APIPath, APIKey, ModelName: string);
    destructor Destroy; override;

    procedure Restart; overload; override;
    procedure Restart(ASysPrompt: string); overload; override;

    function Chat(const AInput: string): Integer; override;
    procedure AbortGeneration; override;

  protected
    function GetOutputAcc: string; override;
    function GetThoughtAcc: string; override;
    procedure SetAIPrefix(Value: string); override;
  private
    procedure _Run;
    procedure HandleHttpData(Sender : TObject; Flush: Boolean);
{$if defined(fpc)}
    procedure HttpData(Sender : TObject; Const ContentLength, CurrentPos : Int64);
{$elseif defined(dcc)}
    procedure HttpData(const Sender: TObject; ContentLength, CurrentPos: Int64; var AAbort: Boolean);
{$endif}
    procedure SyncedCallDone;
    procedure SyncedReportData;
    procedure NotifyData(AData: string);
    function  BuildRequest: string;
    procedure NotifyChunk(S: string);
    procedure NotifyThoughtChunk(S: string);
  end;

function _RunThreadedTask(Parameter : Pointer): IntPtr;
begin
  TOpenAILLM(Parameter)._Run;
  Result := 0;
end;

{$if defined(dcc)}
procedure InitCriticalSection(var CS: TCriticalSection);
begin
  CS := TCriticalSection.Create;
end;

procedure DoneCriticalSection(var CS: TCriticalSection);
begin
  FreeAndNil(CS);
end;

procedure EnterCriticalSection(CS: TCriticalSection);
begin
  CS.Enter;
end;

procedure LeaveCriticalSection(CS: TCriticalSection);
begin
  CS.Leave;
end;

{$endif}

{ TOpenAILLM }

procedure TOpenAILLM.SetBusy(AValue: Boolean);
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self, AValue);
end;

constructor TOpenAILLM.Create(APIPath, APIKey, ModelName: string);
begin
  inherited Create;
  InitCriticalSection(FCS);

{$if defined(fpc)}
  FHTTP := TFPHTTPClient.Create(nil);
  FHTTP.AddHeader('Content-Type', 'application/json; charset=UTF-8');
  FHTTP.AddHeader('Authorization', 'Bearer ' + APIKey);
  FHTTP.AddHeader('Accept', 'text/event-stream');
  FHTTP.OnDataReceived := HttpData;
{$elseif defined(dcc)}
  FHTTP := THTTPClient.Create;
  FHTTP.CustHeaders.Add('Content-Type', 'application/json; charset=UTF-8');
  FHTTP.CustHeaders.Add('Authorization', 'Bearer ' + APIKey);
  FHTTP.CustHeaders.Add('Accept', 'text/event-stream');
  FHTTP.OnReceiveData := HttpData;
{$endif}

  FData := TStringList.Create;
  FAPIPath := APIPath;
  FReadPos := 0;
  FStop := '';
  FModelName := ModelName;

  FResponseStream := TMemoryStream.Create;
end;

destructor TOpenAILLM.Destroy;
begin
  FResponseStream.Free;
  FData.Free;
  FHTTP.Free;
  DoneCriticalSection(FCS);
  inherited Destroy;
end;

procedure TOpenAILLM.Restart;
begin
  if Length(FHistory) < 1 then Exit;
  if FHistory[0].Role = 'system' then
    SetLength(FHistory, 1)
  else
    SetLength(FHistory, 0);
end;

procedure TOpenAILLM.Restart(ASysPrompt: string);
begin
  if ASysPrompt <> '' then
  begin
    SetLength(FHistory, 1);
    FHistory[0].Role := 'system';
    FHistory[0].Content := ASysPrompt;
  end
  else
    SetLength(FHistory, 0);
end;

function TOpenAILLM.Chat(const AInput: string): Integer;
begin
  Result := 0;
  FReadPos := 0;
  SetBusy(True);

  if FOutputAcc <> '' then
  begin
    SetLength(FHistory, Length(FHistory) + 1);
    with FHistory[High(FHistory)] do
    begin
      Role := 'assistant';
      Content := FOutputAcc;
    end;
  end;

  FOutputAcc := '';
  FThoughtAcc := '';

  SetLength(FHistory, Length(FHistory) + 1);
  with FHistory[High(FHistory)] do
  begin
    Role := 'user';
    Content := AInput;
  end;
  if FAIPrefix <> '' then
    OnChunk(Self, FAIPrefix);

{$if defined(fpc)}
  FHTTP.Terminate;
{$elseif defined(dcc)}
  FAbort := False;
{$endif}

  FResponseStream.Clear;

{$if defined(fpc)}
  if BeginThread(@_RunThreadedTask, Self) = 0 then
  begin
    SetBusy(False);
    Result := -1;
  end;
{$elseif defined(dcc)}
  var T := TTask.Create(procedure
    begin
      _RunThreadedTask(Self);
    end);
  T.Start;
  
{$endif}

end;

procedure TOpenAILLM.AbortGeneration;
begin
  EnterCriticalSection(FCS);
{$if defined(fpc)}
  if Assigned(FHttp) then FHttp.Terminate;
{$elseif defined(dcc)}
  FAbort := True;
{$endif}
  LeaveCriticalSection(FCS);
end;

function TOpenAILLM.GetOutputAcc: string;
begin
  Result := FOutputAcc;
end;

function TOpenAILLM.GetThoughtAcc: string;
begin
  Result := FThoughtAcc;
end;

procedure TOpenAILLM.HandleHttpData(Sender: TObject; Flush: Boolean);
var
  T: Int64;
  S: AnsiString;
  P: PAnsiChar;
  _Start: Int64;
  Ended: Int64;
  Dirty: Boolean;
begin
  S := '';
  Dirty  := False;

  P := PAnsiChar(FResponseStream.Memory);

  while True do
  begin
    T := FReadPos;

    _Start := -1;
    Ended := -1;

    while T < FResponseStream.Size do
    begin
      if not (P[T] in [#10, #13]) then
      begin
        _Start := T;
        Break;
      end;
      Inc(T);
    end;

    if _Start < 0 then Break;

    while T < FResponseStream.Size do
    begin
      if (P[T] in [#10, #13]) then
      begin
        Ended := T;
        Break;
      end;
      Inc(T);
    end;

    if Flush and (Ended < 0) then
    begin
      Ended := FResponseStream.Size;
    end;

    if Ended <= _Start then Break;

    FReadPos := Ended;
    Dirty := True;

    SetLength(S, Ended - _Start);
    Move(P[_Start], S[1], Ended - _Start);

    EnterCriticalSection(FCS);
{$if defined(fpc)}
    FData.Add(S);
{$elseif defined(dcc)}
    FData.Add(UTF8ToWideString(S));
{$endif}
    LeaveCriticalSection(FCS);
  end;

  if Dirty then
    TThread.Synchronize(nil, SyncedReportData);
end;

procedure TOpenAILLM.SetAIPrefix(Value: string);
begin
  FAIPrefix := Value;
end;

procedure TOpenAILLM._Run;
{$if defined(dcc)}
var
  Request: TStringStream;
{$endif}
begin
  try
    try
{$if defined(fpc)}
      FHTTP.RequestBody := TRawByteStringStream.Create(BuildRequest);
      FHTTP.Post(FAPIPath, FResponseStream);
{$elseif defined(dcc)}
      Request := TStringStream.Create(BuildRequest);
      FHTTP.Post(FAPIPath, Request, FResponseStream);
{$endif}
    except
    end
  finally
{$if defined(dcc)}
    Request.Free;
{$endif}
  end;

  HandleHttpData(Self, True);

  TThread.Synchronize(nil, SyncedCallDone);
end;

{$if defined(fpc)}
procedure TOpenAILLM.HttpData(Sender : TObject; Const ContentLength, CurrentPos : Int64);
begin
  HandleHttpData(Sender, False);
end;
{$elseif defined(dcc)}
procedure TOpenAILLM.HttpData(const Sender: TObject; ContentLength, CurrentPos: Int64; var AAbort: Boolean);
begin
  if FAbort then
  begin
    AAbort := FAbort;
    Exit;
  end;
  HandleHttpData(Sender, False);
end;
{$endif}

procedure TOpenAILLM.SyncedCallDone;
begin
  if FStop <> '' then
    NotifyChunk(FStop);
  SetBusy(False);
end;

procedure TOpenAILLM.SyncedReportData;
var
  S: string;
  Ready: Boolean;
begin
  while True do
  begin
    Ready := False;

    EnterCriticalSection(FCS);
    if FData.Count > 0 then
    begin
      S := FData[0];
      FData.Delete(0);
      Ready := True;
    end;
    LeaveCriticalSection(FCS);

    if not Ready then Break;
    NotifyData(S);
  end;
end;

procedure TOpenAILLM.NotifyData(AData: string);
var
  O: ISuperObject;
  S: string;
begin
  if not AData.StartsWith('data:') then
  begin
    if AData = ': keep-alive' then Exit;
    if AData.StartsWith(': OPENROUTER') then Exit;
    Exit;
  end;

  Delete(AData, 1, 6);
  O := SO(AData);
  S := O.S['choices[0].delta.content'];
  if S <> '' then
  begin
    NotifyChunk(S);
    Exit;
  end;

  S := O.S['choices[0].delta.reasoning_content'];
  if S <> '' then
    NotifyThoughtChunk(S);
end;

function TOpenAILLM.BuildRequest: string;
var
  R: ISuperObject;
  O: ISuperObject;
  A: TSuperArray;
  M: TChatMessage;
begin
  R := SO(Format('{"model": "%s","messages": [], "stream": true}', [FModelName]));
  if FStop <> '' then
    R.S['stop'] := FStop;

  A := R.A['messages'];
  for M in FHistory do
  begin
    O := SO();
    O.S['role'] := M.Role;
    O.S['content'] := M.Content;
    A.Add(O);
  end;
  if FAIPrefix <> '' then
  begin
    O := SO();
    O.S['role'] := 'assistant';
    O.S['content'] := FAIPrefix;
    O.B['prefix'] := True;
    A.Add(O);
  end;
  Result := R.AsJSon(True);
end;

procedure TOpenAILLM.NotifyChunk(S: string);
begin
  FOutputAcc := FOutputAcc + S;
  if Assigned(FOnChunk) then
    FOnChunk(Self, S);
end;

procedure TOpenAILLM.NotifyThoughtChunk(S: string);
begin
  FThoughtAcc := FThoughtAcc + S;
  if Assigned(FOnThoughtChunk) then
    FOnThoughtChunk(Self, S);
end;

{ TLLMFactory }

class function TLLMFactory.Create(AOptions: ISuperObject): TBaseChatLLM;
var
  A: TSuperArray;
  C: TChatLLM;
  I: Integer;
begin
  Result := nil;
  if AOptions.IsType(stArray) then
  begin
    C := TChatLLM.Create;
    A := AOptions.AsArray;
    for I := 0 to A.Length - 1 do
       C.AddParam(string(A.S[I]));
    Result := C;
  end
  else if AOptions.IsType(stObject) then
  begin
    Result := TOpenAILLM.Create(AOptions.S['url'], AOptions.S['key'], AOptions.S['model']);
  end
end;

end.

