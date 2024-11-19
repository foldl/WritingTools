unit UnitTools;

{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
  Types, Classes, Graphics, SysUtils, Windows, Dialogs, Forms, ExtCtrls, StdCtrls,
  Clipbrd, Controls,
  MarkdownProcessor, superobject, LibChatLLM,
{$if defined(dcc)}
  System.Generics.Collections, System.IOUtils, System.UITypes,
  System.NetEncoding, System.Threading
{$elseif defined(fpc)}
  Generics.Collections, LazUtils
{$endif}
  ;

const
  CARD_MAIN     = 0;
  CARD_RUNNING  = 1;
  CARD_CHAT     = 2;
  CARD_SHOW     = 3;

type
  TWritingActionType = (watPrepend, watReplace, watAppend, watShow, watClipboard);

  TWritingAction = class
  private
    FActionType: TWritingActionType;
    FOriginalActionType: TWritingActionType;
    FExtractCode: Boolean;
    FSysPrompt: string;
    FOriginalPrompt: string;
    FPrompt: string;
    FShortcut: Char;
    FName: string;
    FAIPrefix: string;
    FAISuffix: string;
    FLLMName: string;
  public
    constructor Create(O: ISuperObject);

  public
    property Name: string read FName;
    property ActionType: TWritingActionType read FActionType write FActionType;
    property OriginalActionType: TWritingActionType read FOriginalActionType;
    property ExtractCode: Boolean read FExtractCode;
    property Shortcut: Char read FShortcut;
    property SysPrompt: string read FSysPrompt;
    property Prompt: string read FPrompt write FPrompt;
    property OriginalPrompt: string read FOriginalPrompt;
    property LLMName: string read FLLMName;
  end;

  TThemeBackground = record
    Color1: TColor;
    Color2: TColor;
  end;

  TOSHotkey = class
  private
    FHotkeyID: Integer;
    FEnabled: Boolean;
    FWnd: HWND;
    FModifier: UInt32;
    FKey: UInt32;
    procedure SetHotkey(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    function GetHotkey: string;
  public
    constructor Create(AHandle: HWND);
    destructor Destroy; override;
    property Hotkey: string read GetHotkey write SetHotkey;
    property Enabled: Boolean  read FEnabled write SetEnabled;
  end;

  THotkeyConfig = record
    Delay1: Integer;
    Delay2: Integer;
    Delay3: Integer;
  end;

  TUIConfig = record
    Background: TThemeBackground;
    Width: Integer;
    Height: Integer;
  end;

  TLLMContext = class
  public
    Params: TStringList;
    LLM: TChatLLM;
    Status: Integer;
  public
    constructor Create;
    function Load: Integer;
    destructor Destroy; override;
  end;

  TWritingProfile = class
  private
    FName: string;
    FFileName: string;
    FActions: TObjectList<TWritingAction>;
    FHotkeyConfig: THotkeyConfig;

    FLLMs: TDictionary<string, TLLMContext>;
    FTitle: string;

    procedure Save;
    function GetAction(Index: Integer): TWritingAction;
    function GetLLM(Action: TWritingAction): TChatLLM;
    function GetActionNumber: Integer;
  public
    UIConfig: TUIConfig;
    FQuickChatAction: TWritingAction;
    FCustomAction: TWritingAction;
    FHotkey: TOSHotkey;

    constructor Create(AHandle: HWND; AFileName: string); overload;
    destructor Destroy; override;

    procedure LoadLLM(OnLLMPrint: TLLMPrintEvent; OnLLMStateChanged: TLLMStateChangedEvent);

    property Name: string read FName;
    property Title: string read FTitle;

    property HotkeyConfig: THotkeyConfig read FHotkeyConfig;
    property ActionNumber: Integer read GetActionNumber;
    property Action[Index: Integer]: TWritingAction read GetAction;
    property LLM[Action: TWritingAction]: TChatLLM read GetLLM;

  end;

  TCallJs = record
    ParamNum: Integer;
    FunName: string;
    Param: string;
  end;

  IWritingToolsUI = interface
    procedure ShowChat;
    procedure ShowPage(Id: Integer);
    procedure UpdateAIMemo(L: string);
    procedure ShowStatus(Msg: string);
    procedure LLMStateChanged(Sender: TObject; ABusy: Boolean);
    procedure LLMLoaded;
  end;

  { TWritingTools }

  TWritingTools = class
  private
    FProfile: TWritingProfile;
    FMdProcessor: TMarkdownProcessor;
    FTemplate1: string;
    FTemplate2: string;
    FForm: TCustomForm;
    FUI: IWritingToolsUI;
    FActiveCardIndex: Integer;
    FContext: string;
    FOutputAcc: string;
    FOutputLinesAcc: TArray<string>;
    FCurAction: TWritingAction;
    FCurLLM: TChatLLM;
    FClipboardBackup: string;
    FChatMode: Boolean;
    FChatHistory: string;

    procedure SetActiveCardIndex(const Value: Integer);
    property ActiveCardIndex: Integer read FActiveCardIndex write SetActiveCardIndex;
    procedure PrepareChat;

    procedure ChatAddUserInput(S: string);
    procedure CompleteAction;
    function FormatUserInput(const S: string): string;
    function FormatAIOutput(const S: string): string;
    procedure SaveBotOutput;

    procedure LLMChunk(Sender: TObject; S: string);
    procedure LLMStateChanged(Sender: TObject; ABusy: Boolean);

    function TemplateQuote(S: string): string;
    procedure LLMLoaded;

  public

    EditCustomPrompt: TCustomEdit;

    procedure EditCustomPromptKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonClearClick(Sender: TObject);

    procedure DoAction(Id: Integer);
  public
    constructor Create(AForm: TCustomForm; AUI: IWritingToolsUI; ProfileFn: string);
    destructor Destroy; override;
    procedure Startup;

    procedure About;

    procedure UserHotkey;

    procedure AbortGeneration;

    function RenderChat: string;

    property Profile: TWritingProfile read FProfile;
  end;

function ParseActionType(S: string; var T: TWritingActionType): Boolean;
function ParseActionTypeDef(S: string; Def: TWritingActionType = watReplace): TWritingActionType;

procedure SimulateCopy(KeyConfig: THotkeyConfig);
procedure SimulatePaste(KeyConfig: THotkeyConfig);

function SafeGetClipboard(var S: string; Retry: Integer = 10): Boolean;

implementation

{$ifndef dcc}
function GetClipboardSequenceNumber: DWORD; external user32 name 'GetClipboardSequenceNumber';
{$endif}

procedure WriteToUTF8TextFile(FN: string; Content: WideString);
var
  F: TFileStream;
  A: AnsiString;
begin
  F := nil;
  A := UTF8Encode(Content);
  try
    F := TFileStream.Create(Fn, fmOpenWrite or fmCreate);
    F.Write(A[1], Length(A));
  finally
    F.Free;
  end;
end;

function ReadFullUTF8TextFile(FN: string): string;
const
  UTF8_BOM: array [0..2] of UInt8 = ($EF, $BB, $BF);
var
  F: TFileStream;
  A: AnsiString;
  BOM: array [0..2] of UInt8;
  S: Int64;
begin
  F := nil;
  try
    F := TFileStream.Create(Fn, fmOpenRead or fmShareDenyNone);
    S := F.Size;
    // skp UTF8 BOM if found
    if S >= SizeOf(BOM) then
    begin
      F.Read(BOM[0], SizeOf(BOM));
      if not CompareMem(@BOM[0], @UTF8_BOM[0], SizeOf(BOM)) then
        F.Seek(0, soBeginning)
      else
        Dec(S, SizeOf(BOM));
    end;
    SetLength(A, S);
    F.Read(A[1], S);
  finally
    F.Free;
  end;
  Result := string(A);
end;

function ParseUTF8JsonFile(FN: string): ISuperObject;
var
  Content: string;
begin
  Content := ReadFullUTF8TextFile(FN);
{$ifdef dcc}
  Result := TSuperObject.ParseString(PChar(Content), False);
{$else}
  Result := TSuperObject.ParseString(PWideChar(UnicodeString(Content)), False);
{$endif}
end;

function SafeGetClipboard(var S: string; Retry: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := 0;
  while I < Retry do
  begin
    Inc(I);
    try
      S := Clipboard.AsText;
      Result := True;
      Break;
    except
      Sleep(80);
    end
  end;
end;

procedure SafeSetClipboard(Text: string; Retry: Integer = 2);
var
  I: Integer;
begin
  I := 0;
  while I < Retry do
  begin
    Inc(I);
    try
      Clipboard.AsText := Text;
      Break;
    except
      Sleep(50);
    end;
  end;
end;

function SendKeyEvent(Vk: Word; Up: Boolean; Delay: Integer = 100): Integer;
var
  K: TInput;
begin
  FillChar(K, SizeOf(K), 0);
  with K do
  begin
{$ifdef dcc}
    Itype := INPUT_KEYBOARD;
{$else}
    _Type := INPUT_KEYBOARD;
{$endif}
    ki.wVk := 0;
    ki.wScan := Word(MapVirtualKey(Vk, 0));
    ki.dwFlags := KEYEVENTF_SCANCODE;
    if Up then
      ki.dwFlags := ki.dwFlags or KEYEVENTF_KEYUP;
{$ifdef dcc}
    ki.dwExtraInfo := GetMessageExtraInfo();
{$endif}
  end;
{$ifdef dcc}
  Result := Integer(SendInput(1, K, SizeOf(K)));
{$else}
  Result := Integer(SendInput(1, @K, SizeOf(K)));
{$endif}
  Sleep(Delay);
end;

procedure SimulateCopy(KeyConfig: THotkeyConfig);
begin
  Sleep(KeyConfig.Delay1);
  SendKeyEvent(VK_CONTROL, False, KeyConfig.Delay2);
  SendKeyEvent(Ord('C'), False, KeyConfig.Delay2);
  SendKeyEvent(Ord('C'), True, KeyConfig.Delay2);
  SendKeyEvent(VK_CONTROL, True, KeyConfig.Delay2);
end;

procedure SimulatePaste(KeyConfig: THotkeyConfig);
begin
  Sleep(KeyConfig.Delay1);
  SendKeyEvent(VK_CONTROL, False, KeyConfig.Delay2);
  SendKeyEvent(Ord('V'), False, KeyConfig.Delay2);
  SendKeyEvent(Ord('V'), True, KeyConfig.Delay2);
  SendKeyEvent(VK_CONTROL, True, KeyConfig.Delay2);
end;

function ParseActionType(S: string; var T: TWritingActionType): Boolean;
begin
  S := LowerCase(S);
  Result := True;

  if S = 'prepend' then T := watPrepend
  else if S = 'append' then T := watAppend
  else if S = 'show' then T := watShow
  else if S = 'clipboard' then T := watClipboard
  else if S = 'replace' then T := watReplace
  else Result := False;
end;

function ParseActionTypeDef(S: string; Def: TWritingActionType = watReplace): TWritingActionType;
begin
  S := LowerCase(S);
  Result := Def;

  if S = 'prepend' then Result := watPrepend
  else if S = 'append' then Result := watAppend
  else if S = 'show' then Result := watShow
  else if S = 'clipboard' then Result := watClipboard
  else;
end;

{ TWritingProfile }

constructor TWritingProfile.Create(AHandle: HWND; AFileName: string);
var
  O: ISuperObject;
  LLMs: TSuperTableString;
  S: TSuperAvlEntry;
  I: Integer;
  A: TSuperArray;
  Ctx: TLLMContext;

  function ParseHexColorDef(S: string; Def: TColor): TColor;
  var
    V: UInt;
  begin
    if S = '' then Exit(Def);
    if not TryStrToUInt('0x' + S, V) then Exit(Def);
    Result := TColor(V);
  end;
begin
  FFileName := AFileName;
  FActions := TObjectList<TWritingAction>.Create;
  FLLMs := TDictionary<string, TLLMContext>.Create;
  FHotKey := TOSHotkey.Create(AHandle);

  O := ParseUTF8JsonFile(FFileName);

  FName := string(O.S['name']);
  FTitle := string(O.S['title']);
  if FTitle = '' then FTitle := 'Writing Tools';

  FHotkey.Hotkey := string(O.S['hotkey']);
  LLMs := O.O['chatllm'].AsObject;
  for S in LLMs do
  begin
    Ctx := TLLMContext.Create;
    A := S.Value.AsArray;
    for I := 0 to A.Length - 1 do
       Ctx.Params.Add(string(A.S[I]));

    FLLMs.Add(string(S.Name), Ctx);
  end;

  UIConfig.Background.Color1 := ParseHexColorDef(string(O.S['ui.background.color1']), TColor($ccaaaa));
  UIConfig.Background.Color2 := ParseHexColorDef(string(O.S['ui.background.color2']), clWhite);

  UIConfig.Width := O.I['ui.width'];
  UIConfig.Height := O.I['ui.height'];

  if UIConfig.Width = 0 then UIConfig.Width := 450;
  if UIConfig.Height = 0 then UIConfig.Height := 350;

  if UIConfig.Width < 100 then UIConfig.Width := 100;
  if UIConfig.Height < 100 then UIConfig.Height := 100;

  FHotkeyConfig.Delay1 := StrToIntDef(string(O.S['ui.hotkey.delay1']), 200);
  FHotkeyConfig.Delay2 := StrToIntDef(string(O.S['ui.hotkey.delay2']), 40);
  FHotkeyConfig.Delay3 := StrToIntDef(string(O.S['ui.hotkey.delay3']), 100);

  FCustomAction := TWritingAction.Create(O.O['custom']);
  FQuickChatAction := TWritingAction.Create(O.O['quick-chat']);

  A := O.O['actions'].AsArray;
  for I := 0 to A.Length - 1 do
  begin
    FActions.Add(TWritingAction.Create(A.O[I]));
  end;
end;

destructor TWritingProfile.Destroy;
begin
  Save;
  FHotkey.Free;
  FLLMs.Free;
  inherited;
end;

function TWritingProfile.GetAction(Index: Integer): TWritingAction;
begin
  if Index < 0 then Exit(FCustomAction);
  if Index <= FActions.Count - 1 then Exit(FActions[Index]);
  Result := nil;
end;

function TWritingProfile.GetActionNumber: Integer;
begin
  Result := FActions.Count;
end;

function TWritingProfile.GetLLM(Action: TWritingAction): TChatLLM;
const
  DEF_LLM = 'default';
var
  N: string;
begin
  Result := nil;
  if not Assigned(Action) then Exit;

  N := Action.LLMName;
  if FLLMs.ContainsKey(N) then Result := FLLMs[N].LLM;
  if FLLMs.ContainsKey(DEF_LLM) then Result := FLLMs[DEF_LLM].LLM;
end;

procedure TWritingProfile.LoadLLM(OnLLMPrint: TLLMPrintEvent;
  OnLLMStateChanged: TLLMStateChangedEvent);
var
  LLM: TPair<string, TLLMContext>;
begin
  for LLM in FLLMs do
  begin
    LLM.Value.Load;
    if LLM.Value.Status = 0 then
    begin
      LLM.Value.LLM.OnChunk := OnLLMPrint;
      LLM.Value.LLM.OnStateChanged := OnLLMStateChanged;
    end;
  end;
end;

procedure TWritingProfile.Save;
var
  O: ISuperObject;
begin
  O := ParseUTF8JsonFile(FFileName);
  O.I['ui.width'] := UIConfig.Width;
  O.I['ui.height'] := UIConfig.Height;
  WriteToUTF8TextFile(FFileName, O.AsJSon(True, True));
end;

{ TWritingAction }

constructor TWritingAction.Create(O: ISuperObject);
var
  S: string;
begin
  FName := string(O.S['name']);
  FSysPrompt := string(O.S['sys_prompt']);
  FPrompt := string(O.S['prompt']);
  FOriginalPrompt := FPrompt;
  FActionType := ParseActionTypeDef(string(O.S['action']));
  FExtractCode := O.B['code'];
  FLLMName := string(O.S['llm']);
  FOriginalActionType := FActionType;
  FAIPrefix := string(O.S['ai_prefix']);
  FAISuffix := string(O.S['ai_suffix']);

  S := string(O.S['accelerator']);
  if Length(S) = 1 then
    FShortcut := UpperCase(S)[1];
end;

{ TOSHotkey }

constructor TOSHotkey.Create(AHandle: HWND);
begin
  FWnd := AHandle;
{$ifdef dcc}
  FHotKeyID := GlobalAddAtom(PWideChar(Format('Hotkey@%p', [Pointer(Self)])));
{$else}
  FHotKeyID := GlobalAddAtom(PChar(Format('Hotkey@%p', [Pointer(Self)])));
{$endif}
end;

destructor TOSHotkey.Destroy;
begin
  Enabled := False;
  inherited;
end;

function TOSHotkey.GetHotkey: string;
begin
  if FKey = 0 then Exit('');
  Result := '';
  if (FModifier and MOD_WIN) <> 0 then Result := Result + 'win+';
  if (FModifier and MOD_CONTROL) <> 0 then Result := Result + 'ctrl+';
  if (FModifier and MOD_ALT) <> 0 then Result := Result + 'alt+';
  if (FModifier and MOD_SHIFT) <> 0 then Result := Result + 'shift+';
  Result := Result + LowerCase(Chr(FKey));
end;

procedure TOSHotkey.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  if FEnabled then
    UnregisterHotKey(FWnd, FHotkeyID);

  FEnabled := Value;

  if FEnabled then
  begin
    if FKey > 0 then
      FEnabled := RegisterHotKey(FWnd, FHotkeyID, FModifier, FKey)
    else
      FEnabled := False;
  end;
end;

procedure TOSHotkey.SetHotkey(const Value: string);
var
  E: Boolean;
  S: string;
  SS: string;
begin
  if SameText(Hotkey, Value) then Exit;

  E := Enabled;
  if E then Enabled := False;

  FModifier := 0;
  FKey := 0;
  for S in Value.Split(['+', '-']) do
  begin
    if Length(S) < 1 then Continue;

    SS := UpperCase(S);
    if SS = 'CTRL' then FModifier := FModifier or MOD_CONTROL
    else if SS = 'ALT' then FModifier := FModifier or MOD_ALT
    else if SS = 'SHIFT' then FModifier := FModifier or MOD_SHIFT
    else if SS = 'WIN' then FModifier := FModifier or MOD_WIN
    else if Length(SS) = 1 then
      FKey := Ord(SS[1]);
  end;

  if E then Enabled := True;
end;

{ TLLMContext }

constructor TLLMContext.Create;
begin
  Params := TStringList.Create;
end;

destructor TLLMContext.Destroy;
begin
  Params.Free;
  LLM.Free;
  inherited;
end;

function TLLMContext.Load: Integer;
begin
  Result := Status;
  if Assigned(LLM) then Exit;

  LLM := TChatLLM.Create;
  LLM.AddParam(Params);
  Status := LLM.Start;
  if Status <> 0 then
  begin
    MessageDlg(Format('Failed to load LLM using params: %s', [Params.Text]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
  end;
end;

{ TWritingTools }

procedure TWritingTools.AbortGeneration;
begin
  if Assigned(FCurLLM) then
    FCurLLM.AbortGeneration;
end;

procedure TWritingTools.About;
begin
  MessageDlg('Writing Tools v1.2'#10#10'Written in Delphi/FPC. Powered by ChatLLM.cpp', TMsgDlgType.mtInformation, [mbOK], -1);
end;

procedure TWritingTools.ButtonClearClick(Sender: TObject);
begin
  FChatHistory := '';
  FOutputAcc   := '';
  FUI.ShowChat;
end;

procedure TWritingTools.ChatAddUserInput(S: string);
begin
  FChatHistory := FChatHistory + FormatUserInput(S);
end;

procedure TWritingTools.CompleteAction;
const
  ERR_MSG = 'ERROR_TEXT_INCOMPATIBLE_WITH_REQUEST';
var
  Combined: string;
begin
  if FOutputAcc.IndexOf(ERR_MSG) >= 0 then
  begin
    SafeSetClipboard(FClipboardBackup);
    Exit;
  end;

  if FCurAction.ActionType = watShow then
  begin
    SafeSetClipboard(FClipboardBackup);
    Exit;
  end;

  if FCurAction.ActionType = watClipboard then
  begin
    SafeSetClipboard(FOutputAcc);
    FForm.Hide;
    Exit;
  end;

  case FCurAction.ActionType of
    watPrepend: Combined := FOutputAcc.TrimRight + #13#10 + FContext; // Windows!
    watReplace: Combined := FOutputAcc;
    watAppend: Combined := FContext.TrimRight + #13#10 + FOutputAcc;
  end;

  SafeSetClipboard(Combined);
  FForm.Hide;

  SimulatePaste(FProfile.HotkeyConfig);
  Sleep(FProfile.HotkeyConfig.Delay3);
  SafeSetClipboard(FClipboardBackup);
end;

constructor TWritingTools.Create(AForm: TCustomForm; AUI: IWritingToolsUI; ProfileFn: string);
var
  P: string;
begin
  FUI := AUI;
  FForm := AForm;
  P := ExtractFilePath(Application.ExeName);
  if FileExists(P + 'data/p1.html') then
  begin
    FTemplate1 := ReadFullUTF8TextFile(P + 'data/p1.html');
    FTemplate2 := ReadFullUTF8TextFile(P + 'data/p2.html');
  end
  else begin
    FTemplate1 := ReadFullUTF8TextFile(P + '../../data/p1.html');
    FTemplate2 := ReadFullUTF8TextFile(P + '../../data/p2.html');
  end;
  FMdProcessor := TMarkdownProcessor.CreateDialect(mdCommonMark);
  FProfile := TWritingProfile.Create(AForm.Handle, ProfileFn);

  FForm.Width := FProfile.UIConfig.Width;
  FForm.Height := FProfile.UIConfig.Height;
  FForm.Caption := FProfile.Title;
  Application.Title := FProfile.Title;
end;

destructor TWritingTools.Destroy;
begin
  if Assigned(FProfile) then
  begin
    FProfile.UIConfig.Width := FForm.Width;
    FProfile.UIConfig.Height := FForm.Height;
  end;
  FProfile.Free;
  FMdProcessor.Free;
  inherited;
end;

procedure TWritingTools.DoAction(Id: Integer);
var
  S: string;
begin
  FChatMode := False;
  if FContext = '' then Exit;

  FOutputAcc := '';

  FCurAction := FProfile.Action[Id];
  FCurLLM := FProfile.LLM[FCurAction];
  if not Assigned(FCurLLM) then
  begin
    FUI.ShowStatus('Unavailable');
    Exit;
  end;
  if FCurAction.ActionType = watShow then
  begin
    FUI.ShowPage(CARD_SHOW);
  end;
  FCurLLM.Restart(FCurAction.SysPrompt);
  FCurLLM.AIPrefix := FCurAction.FAIPrefix;
  FCurLLM.AutoAbortSufffix := FCurAction.FAISuffix;
  S := FCurAction.Prompt;
  S := S.Replace('{context}', FContext);
  FCurLLM.Chat(S);
end;

procedure TWritingTools.EditCustomPromptKeyPress(Sender: TObject;
  var Key: Char);
var
  Prompt: string;
  S: string;

  procedure QuickChat;
  begin
    PrepareChat;
    ChatAddUserInput(Prompt);
    FOutputAcc := '';

    FCurLLM := FProfile.LLM[FProfile.FQuickChatAction];
    if not Assigned(FCurLLM) then
    begin
      FUI.ShowStatus('Unavailable');
      Exit;
    end;
    if not FChatMode then
      FCurLLM.Restart(FProfile.FQuickChatAction.SysPrompt);

    FChatMode := True;
    FCurLLM.Chat(Prompt);
  end;

  procedure CustomAction;
  var
    I: Integer;
    prefix: string;
    T: TWritingActionType;
  begin
    S := FProfile.FCustomAction.OriginalPrompt;
    FProfile.FCustomAction.ActionType := FProfile.FCustomAction.OriginalActionType;

    if Prompt.StartsWith('/') then
    begin
      I := Pos(' ', Prompt);
      if I > 0 then
        prefix := Trim(Copy(Prompt, 2, I - 2));
    end
    else begin
      I := Pos(':', Prompt);
      if I > 0 then
        prefix := Trim(Copy(Prompt, 1, I - 1));
    end;

    if I > 0 then
    begin
      if SameText(prefix, 'chat') then
      begin
        SafeSetClipboard(FClipboardBackup, 1);
        FContext := '';
        FUI.ShowStatus('Quick chat');
        Delete(Prompt, 1, I);
        FOutputAcc := '';
        QuickChat;
        Exit;
      end;

      T := watReplace;
      if ParseActionType(prefix, T) then
      begin
        Delete(Prompt, 1, I);
        FProfile.FCustomAction.ActionType := T;
      end;
    end;

    FProfile.FCustomAction.Prompt := S.Replace('{prompt}', Prompt);
    DoAction(-1);
  end;

begin
  if Ord(Key) <> VK_RETURN then Exit;

  Prompt := Trim(EditCustomPrompt.Text);
  EditCustomPrompt.Text := '';

  if Prompt = '' then Exit;

  if FContext <> '' then
  begin
    CustomAction;
  end
  else begin
    QuickChat;
  end;
end;

function TWritingTools.FormatAIOutput(const S: string): string;
begin
  if S = '' then Exit('');

  Result := Format('<div class="message_container message_bot">' +
  '<div class="avatar_container avatar_bot"><div class="avatar_icon_bot"></div></div>' +
  '<div class="message">%s</div></div>', [FMdProcessor.Process(S)]);
end;

function TWritingTools.FormatUserInput(const S: string): string;
begin
  if S = '' then Exit('');

  Result := Format('<div class="message_container message_user">' +
  '<div class="avatar_container avatar_user"><div class="avatar_icon_user"></div></div>' +
  '<div class="message">%s</div>' +
  '</div>', [FMdProcessor.Process(S)]);
end;

procedure TWritingTools.LLMChunk(Sender: TObject; S: string);
var
  html: string;
begin
  FOutputAcc := FOutputAcc + S;
  FOutputLinesAcc := FOutputAcc.Split([#13, #10]);

  if FChatMode then
  begin
    FUI.ShowChat;
    Exit;
  end;

  if Assigned(FCurAction) and (FCurAction.ActionType = watShow) then
  begin
    ActiveCardIndex := CARD_SHOW;
    html := FMdProcessor.process(FOutputAcc);
    FUI.UpdateAIMemo(TemplateQuote(html));
    Exit;
  end;

  FUI.ShowStatus(FOutputLinesAcc[High(FOutputLinesAcc)]);
end;

procedure TWritingTools.LLMStateChanged(Sender: TObject; ABusy: Boolean);
begin
  if ABusy and (FActiveCardIndex = CARD_MAIN) then
    ActiveCardIndex := CARD_RUNNING;
  if (not ABusy) and (ActiveCardIndex = CARD_RUNNING) then
    ActiveCardIndex := CARD_MAIN;

  EditCustomPrompt.Enabled := not ABusy;
  if (not ABusy) and Assigned(FCurAction) then
  begin
    CompleteAction;
    FCurAction := nil;
  end;

  if FChatMode then
  begin

    if not ABusy then
    begin
      SaveBotOutput;
      EditCustomPrompt.SetFocus;
    end;
  end;

  FUI.LLMStateChanged(Sender, ABusy);
end;

procedure TWritingTools.PrepareChat;
begin
  ActiveCardIndex := CARD_CHAT;
end;

function TWritingTools.RenderChat: string;
begin
  Result := TemplateQuote(FChatHistory + FormatAIOutput(FOutputAcc));
end;

procedure TWritingTools.SaveBotOutput;
begin
  if Length(FOutputAcc) < 1 then Exit;

  FChatHistory := FChatHistory + FormatAIOutput(FOutputAcc);
end;

procedure TWritingTools.SetActiveCardIndex(const Value: Integer);
begin
  FActiveCardIndex := Value;
  FUI.ShowPage(Value);
end;

{$ifndef dcc}
function _RunThreadedTask(Parameter : Pointer): IntPtr;
var
  O: TWritingTools;
begin
  O := TWritingTools(Parameter);
  O.FProfile.LoadLLM(O.LLMChunk, O.LLMStateChanged);
  TThread.Synchronize(nil, O.LLMLoaded);
  Result := 0;
end;
{$endif}

procedure TWritingTools.Startup;
begin
{$ifdef dcc}
  var T := TTask.Create(
    procedure
    begin
      FProfile.LoadLLM(LLMChunk, LLMStateChanged);
      TThread.Synchronize(TThread.Current, LLMLoaded);
    end);
  T.Start;
{$else}
  BeginThread(_RunThreadedTask, Self);
{$endif}
end;

function TWritingTools.TemplateQuote(S: string): string;
begin
  Result := FTemplate1 + S + FTemplate2;
end;

procedure TWritingTools.LLMLoaded;
begin
  FUI.LLMLoaded;
  FProfile.FHotkey.Enabled := True;
  if not FProfile.FHotkey.Enabled then
  begin
    MessageDlg(Format('Failed to register hotkey: %s', [FProfile.FHotkey.Hotkey]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
    Exit;
  end;
  FUI.ShowStatus('Ready. Press ESC to hide.');
end;

procedure TWritingTools.UserHotkey;
const
  MARGIN = 5;
var
  W1, W2: DWORD;
  Monitor: TMonitor;
begin
  EditCustomPrompt.Text := '';
  ActiveCardIndex := 0;
  if not SafeGetClipboard(FClipboardBackup, 1) then Exit;

  FContext := '';

  W1 := GetClipboardSequenceNumber;

  SimulateCopy(FProfile.HotkeyConfig);

  Sleep(FProfile.HotkeyConfig.Delay3);

  if not SafeGetClipboard(FContext, 1) then Exit;

  W2 := GetClipboardSequenceNumber;

  if W1 = W2 then
    FContext := '';

  if FContext = '' then
    FUI.ShowStatus('(Empty) Go quick chat.')
  else
    FUI.ShowStatus(FContext);

  FOutputAcc := '';

  Monitor := Screen.MonitorFromPoint(TPoint.Create(Mouse.CursorPos.X, Mouse.CursorPos.Y));
  if Mouse.CursorPos.X + MARGIN + FForm.Width < Monitor.Left + Monitor.Width then
    FForm.Left := Mouse.CursorPos.X + MARGIN
  else
    FForm.Left := Mouse.CursorPos.X - FForm.Width - MARGIN;
  if Mouse.CursorPos.Y + MARGIN + FForm.Height < Monitor.Top + Monitor.Height then
    FForm.Top := Mouse.CursorPos.Y + MARGIN
  else
    FForm.Top := Mouse.CursorPos.Y - FForm.Height - MARGIN;

  FForm.Show;
  SetForegroundWindow(FForm.Handle);
  EditCustomPrompt.SetFocus;
end;

end.
