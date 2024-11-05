unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Data.Bind.Components, Vcl.WinXCtrls, Vcl.ExtCtrls,
  System.UITypes, CommCtrl, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  Vcl.Menus, Clipbrd, Vcl.ControlList, Vcl.ComCtrls, GraphUtil, superobject, Generics.Collections,
  LibChatLLM, Vcl.WinXPanels, Winapi.WebView2, Winapi.ActiveX, Vcl.Edge,
  MarkdownProcessor, System.Actions, Vcl.ActnList;

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
    FActions: TObjectList<TWritingAction>;
    FName: string;
    FFileName: string;
    FCustomAction: TWritingAction;
    FQuickChatAction: TWritingAction;
    FHotkeyConfig: THotkeyConfig;
    FHotkey: TOSHotkey;
    FLLMs: TDictionary<string, TLLMContext>;
    FTitle: string;

    procedure Save;
    function GetAction(Index: Integer): TWritingAction;
    function GetLLM(Action: TWritingAction): TChatLLM;
  public
    UIConfig: TUIConfig;

    constructor Create(AHandle: HWND; AFileName: string); overload;
    destructor Destroy; override;

    procedure LoadLLM(OnLLMPrint: TLLMPrintEvent; OnLLMStateChanged: TLLMStateChangedEvent);

    property Name: string read FName;
    property Title: string read FTitle;

    property HotkeyConfig: THotkeyConfig read FHotkeyConfig;

    property Action[Index: Integer]: TWritingAction read GetAction;
    property LLM[Action: TWritingAction]: TChatLLM read GetLLM;

  end;

  TCallJs = record
    ParamNum: Integer;
    FunName: string;
    Param: string;
  end;

  TEdgeBrowserUpdater = class(TComponent)
  private
    FView: TEdgeBrowser;
    FFifo: TQueue<TCallJs>;
    FTimer: TTimer;
    FHtml: string;
    FHtmlPending: Boolean;
    procedure BrowserNavCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
    procedure ScriptExecuted(Sender: TCustomEdgeBrowser; AResult: HResult; const AResultObjectAsJson: string);
    procedure SetBrowser(const Value: TEdgeBrowser; InitialURL: string = 'about:blank');
    procedure DoCall;
    procedure CallJS(FunName, Param: string); overload;
    procedure CallJS(FunName: string); overload;
    procedure TimerTimer(Sender: TObject);
    function GetBusy: Boolean;
  public
    constructor Create(AOwner: TComponent; View: TEdgeBrowser; InitialURL: string = 'about:blank');

    procedure ClearContentAll;
    procedure SetContentAll(Content: string);

    procedure SetPage(const html: string);

    destructor Destroy; override;
    property Browser: TEdgeBrowser read FView;
    property Busy: Boolean read GetBusy;
  end;

  TMainForm = class(TForm)
    TrayIcon1: TTrayIcon;
    Panel2: TPanel;
    EditCustomPrompt: TEdit;
    Panel1: TPanel;
    LabelStatus: TLabel;
    PopupMenu1: TPopupMenu;
    Exit1: TMenuItem;
    Show1: TMenuItem;
    CardPanel: TCardPanel;
    Card1: TCard;
    Card2: TCard;
    GridActions: TGridPanel;
    About1: TMenuItem;
    N1: TMenuItem;
    Card3: TCard;
    Panel3: TPanel;
    ButtonClear: TButton;
    Card4: TCard;
    ActivityIndicator: TProgressBar;
    BrowserForShow: TEdgeBrowser;
    ActionList1: TActionList;
    ActionAbortGeneration: TAction;
    ActionHide: TAction;
    BrowserForChat: TEdgeBrowser;
    procedure Exit1Click(Sender: TObject);
    procedure Hide1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure EditCustomPromptKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonClearClick(Sender: TObject);
    procedure ActionAbortGenerationExecute(Sender: TObject);
    procedure ActionHideExecute(Sender: TObject);
  private
    FProfile: TWritingProfile;
    FContext: string;
    FOutputAcc: string;
    FOutputLinesAcc: TArray<string>;
    FCurAction: TWritingAction;
    FCurLLM: TChatLLM;
    FClipboardBackup: string;
    FChatMode: Boolean;
    FMdProcessor: TMarkdownProcessor;
    FTemplate1: string;
    FTemplate2: string;
    FUpdaterShow: TEdgeBrowserUpdater;
    FUpdaterChat: TEdgeBrowserUpdater;
    procedure ActionButtonClick(Sender: TObject);
    procedure LLMChunk(Sender: TObject; S: string);
    procedure LLMStateChanged(Sender: TObject; ABusy: Boolean);
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure DoAction(Id: Integer);
    procedure CompleteAction;
    procedure PrepareChat;
    procedure UpdateAIMemo(Updater: TEdgeBrowserUpdater; L: string); overload;
  private
    FChatHistory: string;
    function FormatAIOutput(const S: string): string;
    function FormatUserInput(const S: string): string;
    procedure ChatAddUserInput(S: string);
    procedure ShowChat;
    procedure SaveBotOutput;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  CARD_MAIN     = 0;
  CARD_RUNNING  = 1;
  CARD_CHAT     = 2;
  CARD_SHOW     = 3;

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

{ TMainForm }

procedure TMainForm.About1Click(Sender: TObject);
begin
  MessageDlg('Writing Tools v1.1'#10#10'Written in Delphi. Powered by ChatLLM.cpp', TMsgDlgType.mtInformation, [mbOK], -1);
end;

procedure TMainForm.ActionAbortGenerationExecute(Sender: TObject);
begin
  if Assigned(FCurLLM) then
    FCurLLM.AbortGeneration;
end;

procedure TMainForm.ActionButtonClick(Sender: TObject);
begin
  if not (Sender is TButton) then Exit;

  DoAction((Sender as TButton).Tag);
end;

procedure TMainForm.ActionHideExecute(Sender: TObject);
begin
  Hide;
end;

procedure TMainForm.ButtonClearClick(Sender: TObject);
begin
  FChatHistory := '';
  FOutputAcc   := '';
  ShowChat;
end;

procedure TMainForm.DoAction(Id: Integer);
begin
  FChatMode := False;
  if FContext = '' then Exit;

  FOutputAcc := '';

  FCurAction := FProfile.Action[Id];
  FCurLLM := FProfile.LLM[FCurAction];
  if not Assigned(FCurLLM) then
  begin
    LabelStatus.Caption := 'Unavailable';
    Exit;
  end;
  if FCurAction.ActionType = watShow then
  begin
    //FUpdaterShow.CallJS('clearContentAll', '');
    FUpdaterShow.SetPage('<html></html>');
    CardPanel.ActiveCardIndex := CARD_SHOW;
  end;
  FCurLLM.Restart(FCurAction.SysPrompt);
  var S := FCurAction.Prompt;
  S := S.Replace('{context}', FContext);
  FCurLLM.Chat(S);
end;

procedure TMainForm.EditCustomPromptKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Prompt: string;

  procedure QuickChat;
  begin
    PrepareChat;
    ChatAddUserInput(Prompt);

    FCurLLM := FProfile.LLM[FProfile.FQuickChatAction];
    if not Assigned(FCurLLM) then
    begin
      LabelStatus.Caption := 'Unavailable';
      Exit;
    end;
    if not FChatMode then
      FCurLLM.Restart(FProfile.FQuickChatAction.SysPrompt);

    FOutputAcc := '';
    FCurLLM.Chat(Prompt);
    FChatMode := True;
  end;

  procedure CustomAction;
  var
    I: Integer;
    prefix: string;
  begin
    var S := FProfile.FCustomAction.OriginalPrompt;
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
        Clipboard.AsText := FClipboardBackup;
        FContext := '';
        LabelStatus.Caption := 'Quick chat';
        Delete(Prompt, 1, I);
        ShowMessage(Prompt);
        QuickChat;
        Exit;
      end;

      var T: TWritingActionType := watReplace;
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
  if Key <> VK_RETURN then Exit;

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

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

function TMainForm.FormatAIOutput(const S: string): string;
begin
  if S = '' then Exit('');

  Result := Format('''
<div class="message_container message_bot">
  <div class="avatar_container avatar_bot"><div class="avatar_icon_bot"></div></div>
  <div class="message">%s</div>
</div>
''', [FMdProcessor.Process(S)]);
end;

function TMainForm.FormatUserInput(const S: string): string;
begin
  if S = '' then Exit('');
  
  Result := Format('''
<div class="message_container message_user">
  <div class="avatar_container avatar_user"><div class="avatar_icon_user"></div></div>
  <div class="message">%s</div>
</div>
''', [FMdProcessor.Process(S)]);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  var P := ExtractFilePath(Application.ExeName);
  if FileExists(P + 'data/p1.html') then
  begin
    FTemplate1 := TFile.ReadAllText(P + 'data/p1.html');
    FTemplate2 := TFile.ReadAllText(P + 'data/p2.html');
  end
  else begin
    FTemplate1 := TFile.ReadAllText(P + '../../data/p1.html');
    FTemplate2 := TFile.ReadAllText(P + '../../data/p2.html');
  end;
  FMdProcessor := TMarkdownProcessor.CreateDialect(mdCommonMark);

  FUpdaterShow := TEdgeBrowserUpdater.Create(Self, BrowserForShow);
  FUpdaterChat := TEdgeBrowserUpdater.Create(Self, BrowserForChat);

  CardPanel.ActiveCardIndex := CARD_MAIN;

  var FN := P + 'profile.json';
  if not FileExists(FN) then
  begin
    MessageDlg(Format('profile.json not found under path: %s', [FN]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
    Exit;
  end;
  if ParamCount() > 1 then FN := ParamStr(1);
  FProfile := TWritingProfile.Create(Handle, FN);
  Width := FProfile.UIConfig.Width;
  Height := FProfile.UIConfig.Height;
  Caption := FProfile.Title;
  Application.Title := FProfile.Title;

  var RowsNum := (FProfile.FActions.Count + 1) div 2;
  GridActions.RowCollection.Clear;
  for var I := 0 to RowsNum - 1 do
  begin
    GridActions.RowCollection.Add;
  end;
  for var I := 0 to RowsNum - 1 do
  begin
    GridActions.RowCollection.Items[I].SizeStyle := ssPercent;
    GridActions.RowCollection.Items[I].Value := 100 / RowsNum;
  end;

  for var I := 0 to FProfile.FActions.Count - 1 do
  begin
    var B := TButton.Create(GridActions);
    B.Parent := GridActions;
    B.Tag := I;
    B.Align := alClient;
    var C := FProfile.FActions[I].Name;
    if FProfile.FActions[I].Shortcut <> Chr(0) then
      C := C + Format(' (&%s)', [FProfile.FActions[I].Shortcut]);
    B.Caption := C;
    B.OnClick := ActionButtonClick;
  end;

  LabelStatus.Caption := 'loading LLMs';
  FProfile.LoadLLM(LLMChunk, LLMStateChanged);
  ActivityIndicator.Visible := False;
  FProfile.FHotkey.Enabled := True;
  if not FProfile.FHotkey.Enabled then
  begin
    MessageDlg(Format('Failed to register hotkey: %s', [FProfile.FHotkey.Hotkey]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
    Exit;
  end;

  LabelStatus.Caption := 'Ready. Press ESC to hide.';
  SendMessage(ActivityIndicator.Handle, PBM_SETBKCOLOR, 0, FProfile.UIConfig.Background.Color2);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FMdProcessor.Free;
  FProfile.UIConfig.Width := Width;
  FProfile.UIConfig.Height := Height;
  FProfile.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
Var
  R: TRect;
begin
  SetRect(R, 0, 0, Width, Height);
  GradientFillCanvas(Canvas,
    FProfile.UIConfig.Background.Color1,
    FProfile.UIConfig.Background.Color2, R, gdVertical);
end;

procedure TMainForm.Hide1Click(Sender: TObject);
begin
  Visible:= False;
end;

procedure TMainForm.LLMChunk(Sender: TObject; S: string);
begin
  FOutputAcc := FOutputAcc + S;
  FOutputLinesAcc := FOutputAcc.Split([#13, #10]);

  if FChatMode then
  begin
    ShowChat;
    Exit;
  end;

  if Assigned(FCurAction) and (FCurAction.ActionType = watShow) then
  begin
    UpdateAIMemo(FUpdaterShow, FOutputAcc);
    Exit;
  end;

  LabelStatus.Caption := FOutputLinesAcc[High(FOutputLinesAcc)];
end;

procedure TMainForm.LLMStateChanged(Sender: TObject; ABusy: Boolean);
begin
  ActivityIndicator.Visible := ABusy;

  if ABusy and (CardPanel.ActiveCardIndex = CARD_MAIN) then
    CardPanel.ActiveCardIndex := CARD_RUNNING;
  if (not ABusy) and (CardPanel.ActiveCardIndex = CARD_RUNNING) then
    CardPanel.ActiveCardIndex := CARD_MAIN;

  EditCustomPrompt.Enabled := not ABusy;
  if (not ABusy) and Assigned(FCurAction) then
  begin
    CompleteAction;
    FCurAction := nil;
  end;

  if (not ABusy) and FChatMode then
    EditCustomPrompt.SetFocus;
end;

procedure TMainForm.PrepareChat;
begin
  CardPanel.ActiveCardIndex := CARD_CHAT;
end;

procedure TMainForm.SaveBotOutput;
begin
  if Length(FOutputAcc) < 1 then Exit;
  
  FChatHistory := FChatHistory + FormatAIOutput(FOutputAcc);
end;

procedure TMainForm.Show1Click(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.ShowChat;
begin
  FUpdaterChat.SetPage(FTemplate1 + FChatHistory + FormatAIOutput(FOutputAcc) + FTemplate2);
end;

procedure TMainForm.UpdateAIMemo(Updater: TEdgeBrowserUpdater; L: string);
begin
  var html := FMdProcessor.process(L);
  Updater.SetPage(FTemplate1 + html + FTemplate2);
end;

function SendKeyEvent(Vk: Word; Up: Boolean; Delay: Integer = 100): Integer;
var
  K: TInput;
begin
  FillChar(K, SizeOf(K), 0);
  with K do
  begin
    Itype := INPUT_KEYBOARD;
    ki.wVk := 0;
    ki.wScan := Word(MapVirtualKey(Vk, 0));
    ki.dwFlags := KEYEVENTF_SCANCODE;
    if Up then
      ki.dwFlags := ki.dwFlags or KEYEVENTF_KEYUP;
    ki.dwExtraInfo := GetMessageExtraInfo();
  end;
  Result := Integer(SendInput(1, K, SizeOf(K)));
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

procedure SimulateGoLeft(KeyConfig: THotkeyConfig);
begin
  SendKeyEvent(VK_LEFT, False, KeyConfig.Delay2);
  SendKeyEvent(VK_LEFT, True, KeyConfig.Delay2);

  Sleep(KeyConfig.Delay1);

  SendKeyEvent(VK_RETURN, False, KeyConfig.Delay2);
  SendKeyEvent(VK_RETURN, True, KeyConfig.Delay2);
end;

procedure SimulateGoRight(KeyConfig: THotkeyConfig);
begin
  SendKeyEvent(VK_RIGHT, False, KeyConfig.Delay2);
  SendKeyEvent(VK_RIGHT, True, KeyConfig.Delay2);

  Sleep(KeyConfig.Delay1);

  SendKeyEvent(VK_RETURN, False, KeyConfig.Delay2);
  SendKeyEvent(VK_RETURN, True, KeyConfig.Delay2);
end;

procedure TMainForm.ChatAddUserInput(S: string);
begin
  SaveBotOutput;
  FChatHistory := FChatHistory + FormatUserInput(S);
  ShowChat;
end;

procedure TMainForm.CompleteAction;
const
  ERR_MSG = 'ERROR_TEXT_INCOMPATIBLE_WITH_REQUEST';
begin
  if FOutputAcc.IndexOf(ERR_MSG) >= 0 then
  begin
    Clipboard.AsText := FClipboardBackup;
    Exit;
  end;

  if FCurAction.ActionType = watShow then
  begin
    Clipboard.AsText := FClipboardBackup;
    Exit;
  end;

  if FCurAction.ActionType = watClipboard then
  begin
    Clipboard.AsText := FOutputAcc;
    Hide;
    Exit;
  end;

  Clipboard.AsText := FOutputAcc;
  Hide;

  case FCurAction.ActionType of
    watPrepend: SimulateGoLeft(FProfile.HotkeyConfig);
    watReplace: ;
    watAppend: SimulateGoRight(FProfile.HotkeyConfig);
  end;

  SimulatePaste(FProfile.HotkeyConfig);
  Clipboard.AsText := FClipboardBackup;
end;

function SafeGetClipboard(Retry: Integer = 10): string;
var
  I: Integer;
begin
  Result := '';
  I := 0;
  while I < Retry do
  begin
    Inc(I);
    try
      Result := Clipboard.AsText;
      Break;
    except
      Sleep(50);
    end;
  end;
end;

procedure TMainForm.WMHotKey(var Msg: TWMHotKey);
var
  W1, W2: DWORD;
begin
  EditCustomPrompt.Text := '';
  CardPanel.ActiveCardIndex := 0;
  FClipboardBackup := SafeGetClipboard(1);
  FContext := '';

  W1 := GetClipboardSequenceNumber;

  SimulateCopy(FProfile.HotkeyConfig);

  Sleep(FProfile.HotkeyConfig.Delay3);
  W2 := GetClipboardSequenceNumber;

  if W1 <> W2 then
  begin
    FContext := Trim(SafeGetClipboard);
  end;

  if FContext = '' then
    LabelStatus.Caption := '(Empty) Go quick chat.'
  else
    LabelStatus.Caption := FContext;

  FOutputAcc := '';

  Left := Mouse.CursorPos.X + 5;
  Top := Mouse.CursorPos.Y + 5;

  Show;
  SetForegroundWindow(Handle);
  EditCustomPrompt.SetFocus;
end;

{ TWritingProfile }

constructor TWritingProfile.Create(AHandle: HWND; AFileName: string);

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

  var Content := TFile.ReadAllText(FFileName, TEncoding.UTF8);
  var O := TSuperObject.ParseString(PChar(Content), False);
  FName := O.S['name'];
  FTitle := O.S['title'];
  if FTitle = '' then FTitle := 'Writing Tools';

  FHotkey.Hotkey := O.S['hotkey'];
  var LLMs := O.O['chatllm'].AsObject;
  for var S in LLMs do
  begin
    var Ctx := TLLMContext.Create;
    var A := S.Value.AsArray;
    for var I := 0 to A.Length - 1 do
       Ctx.Params.Add(A.S[I]);

    FLLMs.Add(S.Name, Ctx);
  end;

  UIConfig.Background.Color1 := ParseHexColorDef(O.S['ui.background.color1'], TColor($ccaaaa));
  UIConfig.Background.Color2 := ParseHexColorDef(O.S['ui.background.color2'], clWhite);

  UIConfig.Width := O.I['ui.width'];
  UIConfig.Height := O.I['ui.height'];

  if UIConfig.Width = 0 then UIConfig.Width := 450;
  if UIConfig.Height = 0 then UIConfig.Height := 350;

  if UIConfig.Width < 100 then UIConfig.Width := 100;
  if UIConfig.Height < 100 then UIConfig.Height := 100;

  FHotkeyConfig.Delay1 := StrToIntDef(O.S['ui.hotkey.delay1'], 200);
  FHotkeyConfig.Delay2 := StrToIntDef(O.S['ui.hotkey.delay2'], 40);
  FHotkeyConfig.Delay3 := StrToIntDef(O.S['ui.hotkey.delay3'], 100);

  FCustomAction := TWritingAction.Create(O.O['custom']);
  FQuickChatAction := TWritingAction.Create(O.O['quick-chat']);

  var A := O.O['actions'].AsArray;
  for var I := 0 to A.Length - 1 do
  begin
    var Action := TWritingAction.Create(A.O[I]);
    FActions.Add(Action);
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

function TWritingProfile.GetLLM(Action: TWritingAction): TChatLLM;
const
  DEF_LLM = 'default';
begin
  Result := nil;
  if not Assigned(Action) then Exit;

  var N := Action.LLMName;
  if FLLMs.ContainsKey(N) then Result := FLLMs[N].LLM;
  if FLLMs.ContainsKey(DEF_LLM) then Result := FLLMs[DEF_LLM].LLM;
end;

procedure TWritingProfile.LoadLLM(OnLLMPrint: TLLMPrintEvent;
  OnLLMStateChanged: TLLMStateChangedEvent);
begin
  for var LLM in FLLMs do
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
begin
  var Content := TFile.ReadAllText(FFileName, TEncoding.UTF8);
  var O := TSuperObject.ParseString(PChar(Content), False);
  O.I['ui.width'] := UIConfig.Width;
  O.I['ui.height'] := UIConfig.Height;
  TFile.WriteAllText(FFileName, O.AsJSon(True, True), TEncoding.UTF8);
end;

{ TWritingAction }

constructor TWritingAction.Create(O: ISuperObject);
begin
  FName := O.S['name'];
  FSysPrompt := O.S['sys_prompt'];
  FPrompt := O.S['prompt'];
  FOriginalPrompt := FPrompt;
  FActionType := ParseActionTypeDef(O.S['action']);
  FExtractCode := O.B['code'];
  FLLMName := O.S['llm'];
  FOriginalActionType := FActionType;

  var S := O.S['accelerator'];
  if Length(S) = 1 then
    FShortcut := UpperCase(S)[1];
end;

{ TOSHotkey }

constructor TOSHotkey.Create(AHandle: HWND);
begin
  FWnd := AHandle;
  FHotKeyID := GlobalAddAtom(PWideChar(Format('Hotkey@%p', [Pointer(Self)])));
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
begin
  if SameText(Hotkey, Value) then Exit;

  E := Enabled;
  if E then Enabled := False;

  FModifier := 0;
  FKey := 0;
  var L := Value.Split(['+', '-']);
  for var S in L do
  begin
    if Length(S) < 1 then Continue;

    var SS := UpperCase(S);
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

{ TEdgeBrowserUpdater }

procedure TEdgeBrowserUpdater.CallJS(FunName, Param: string);
var
  Call: TCallJs;
begin
  Call.ParamNum := 1;
  Call.FunName := FunName;
  Call.Param   := TNetEncoding.Base64.Encode(Param);
  FFifo.Enqueue(Call);
  DoCall;
end;

procedure TEdgeBrowserUpdater.CallJS(FunName: string);
var
  Call: TCallJs;
begin
  Call.ParamNum := 0;
  Call.FunName := FunName;
  FFifo.Enqueue(Call);
  DoCall;
end;

procedure TEdgeBrowserUpdater.ClearContentAll;
begin
  CallJS('clearContentAll');
end;

constructor TEdgeBrowserUpdater.Create(AOwner: TComponent; View: TEdgeBrowser; InitialURL: string);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1;
  FTimer.OnTimer := TimerTimer;
  FFifo := TQueue<TCallJs>.Create;
  SetBrowser(View, InitialURL);
end;

destructor TEdgeBrowserUpdater.Destroy;
begin
  FFifo.Free;
  inherited;
end;

procedure TEdgeBrowserUpdater.DoCall;
var
  S: string;
begin
  if Busy then Exit;
  if FFifo.IsEmpty then Exit;

  var call := FFifo.Dequeue;

  case call.ParamNum of
    -1: begin
      FView.OnNavigationCompleted := BrowserNavCompleted;
      FView.NavigateToString(call.Param);
    end;
    0: begin
      S := Format('%s();', [call.FunName]);
      FView.OnExecuteScript := ScriptExecuted;
      FView.ExecuteScript(S);
    end;
    1: begin
      S := Format('%s("%s");', [call.FunName, call.Param]);
      FView.OnExecuteScript := ScriptExecuted;
      FView.ExecuteScript(S);
    end
  end;

end;

function TEdgeBrowserUpdater.GetBusy: Boolean;
begin
  Result := Assigned(FView.OnExecuteScript) or Assigned(FView.OnNavigationCompleted);
end;

procedure TEdgeBrowserUpdater.BrowserNavCompleted(Sender: TCustomEdgeBrowser;
  IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
begin
  FView.OnNavigationCompleted := nil;

  if FHtmlPending then
  begin
    FHtmlPending := False;
    FView.OnNavigationCompleted := BrowserNavCompleted;
    FView.NavigateToString(FHtml);
    Exit;
  end;

  FTimer.Enabled := True;
end;

procedure TEdgeBrowserUpdater.ScriptExecuted(Sender: TCustomEdgeBrowser;
  AResult: HResult; const AResultObjectAsJson: string);
begin
  FView.OnExecuteScript := nil;
  FTimer.Enabled := True;
end;

procedure TEdgeBrowserUpdater.SetBrowser(const Value: TEdgeBrowser; InitialURL: string);
begin
  FView := Value;
  if not Assigned(FView) then Exit;

  FView.OnNavigationCompleted := BrowserNavCompleted;
  FView.Navigate(InitialURL);
end;

procedure TEdgeBrowserUpdater.SetContentAll(Content: string);
begin
  CallJS('setContentAll', Content);
end;

procedure TEdgeBrowserUpdater.SetPage(const html: string);
begin
  if Assigned(FView.OnNavigationCompleted) then
  begin
    FHtmlPending := True;
    FHtml := html;
    Exit;
  end;

  FView.OnNavigationCompleted := BrowserNavCompleted;
  FView.NavigateToString(html);
end;

procedure TEdgeBrowserUpdater.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  DoCall;
end;

end.
