unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Data.Bind.Components, Vcl.WinXCtrls, Vcl.ExtCtrls,
  System.UITypes, CommCtrl, System.IOUtils, System.Generics.Collections, System.Threading,
  System.NetEncoding,
  Vcl.Menus, Clipbrd, Vcl.ControlList, Vcl.ComCtrls, GraphUtil, superobject, Generics.Collections,
  LibChatLLM, Vcl.WinXPanels, Winapi.WebView2, Winapi.ActiveX, Vcl.Edge,
  System.Actions, Vcl.ActnList, UnitTools;

type

  TEdgeBrowserUpdater = class(TComponent)
  private
    FView: TEdgeBrowser;
    FTimer: TTimer;
    FPendedCall: TStringList;
    FHtml: string;
    FLastFilePath: string;
    procedure BrowserNavCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
    procedure ScriptExecuted(Sender: TCustomEdgeBrowser; AResult: HResult; const AResultObjectAsJson: string);
    procedure DoCallJs(const S: string);
    procedure CallPendingJs;
    procedure TimerTimer(Sender: TObject);
    function GetBusy: Boolean;
    procedure DoSetPage(const html: string);
  public
    constructor Create(AOwner: TComponent; View: TEdgeBrowser; InitialURL: string = 'about:blank'); reintroduce; overload;

    procedure ResetContent(const FilePath: string = '');
    procedure CallJSFun(const FunName, AParam1, AParam2: string); overload;
    procedure CallJSFun(const FunName, AParam: string); overload;
    procedure CallJSFun(const ALine: string); overload;

    destructor Destroy; override;
    property Browser: TEdgeBrowser read FView;
    property Busy: Boolean read GetBusy;
  end;

  TMainForm = class(TForm, IWritingToolsUI)
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
    ActivityIndicator: TProgressBar;
    ActionList1: TActionList;
    ActionAbortGeneration: TAction;
    ActionHide: TAction;
    PageBrowser: TEdgeBrowser;
    ButtonClear: TButton;
    ButtonRedo: TButton;
    ButtonAccept: TButton;
    procedure Exit1Click(Sender: TObject);
    procedure Hide1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ActionAbortGenerationExecute(Sender: TObject);
    procedure ActionHideExecute(Sender: TObject);
  private
    procedure SetCurrentApp(AValue: string);
  private
    FWritingTools: TWritingTools;
    FBrowserUpdater: TEdgeBrowserUpdater;
    FActButtons: array of TButton;
    FActivePageId: Integer;
    FAppChat: string;
    FChunkAcc: string;
    FIsThought: Boolean;
    procedure FlushAIChunks;
    procedure ActionButtonClick(Sender: TObject);
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure ShowPage(Id: Integer);
    procedure WebAppDiff(A, B: string);
    procedure AppendAIChunk(L: string);
    procedure AppendAIThoughChunk(L: string);
    procedure ChatAddUserInput(L: string);
    procedure ShowStatus(Msg: string);
    procedure LLMStateChanged(Sender: TObject; ABusy: Boolean);
    procedure LLMLoaded;
    procedure ShowChat;
    procedure ClearChatHistory;
    property  CurrentApp: string write SetCurrentApp;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{ TMainForm }

procedure TMainForm.About1Click(Sender: TObject);
begin
  FWritingTools.About;
end;

procedure TMainForm.ActionAbortGenerationExecute(Sender: TObject);
begin
  FWritingTools.AbortGeneration;
end;

procedure TMainForm.ActionButtonClick(Sender: TObject);
begin
  if not (Sender is TButton) then Exit;

  FWritingTools.DoAction((Sender as TButton).Tag);
end;

procedure TMainForm.ActionHideExecute(Sender: TObject);
begin
  Hide;
end;

procedure TMainForm.AppendAIChunk(L: string);
begin
  if FIsThought then FlushAIChunks;
  FChunkAcc := FChunkAcc + L;
  if FBrowserUpdater.Busy then
  begin
    FIsThought := False;
    Exit;
  end;
  FBrowserUpdater.CallJSFun('ai_append_chunk', FChunkAcc);
  FChunkAcc := '';
end;

procedure TMainForm.AppendAIThoughChunk(L: string);
begin
  if not FIsThought then FlushAIChunks;
  FChunkAcc := FChunkAcc + L;
  if FBrowserUpdater.Busy then
  begin
    FIsThought := True;
    Exit;
  end;
  FBrowserUpdater.CallJSFun('ai_append_thought_chunk', FChunkAcc);
  FChunkAcc := '';
end;

procedure TMainForm.ChatAddUserInput(L: string);
begin
  FlushAIChunks;
  FBrowserUpdater.CallJSFun('append_user_input', L);
end;

procedure TMainForm.ClearChatHistory;
begin
  FlushAIChunks;
  FBrowserUpdater.CallJSFun('reset()');
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FlushAIChunks;
begin
  if FChunkAcc = '' then Exit;
  if FIsThought then
    FBrowserUpdater.CallJSFun('ai_append_chunk', FChunkAcc)
  else
    FBrowserUpdater.CallJSFun('ai_append_thought_chunk', FChunkAcc);
  FChunkAcc := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Profile: TWritingProfile;
begin
  var FN := ExtractFilePath(Application.ExeName) + 'profile.json';
  if ParamCount() > 1 then FN := ParamStr(1);

  var BP: string := ExtractFilePath(Application.ExeName)  + 'data' + PathDelim;
  if not DirectoryExists(BP) then
    BP := ExtractFilePath(Application.ExeName) + '../../data' + PathDelim;

  FAppChat := BP + 'app_chat.html';

  if not FileExists(FN) then
  begin
    MessageDlg(Format('profile.json not found: %s', [FN]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
    Exit;
  end;

  FBrowserUpdater := TEdgeBrowserUpdater.Create(Self, PageBrowser);

  CardPanel.ActiveCardIndex := CARD_MAIN;

  FWritingTools := TWritingTools.Create(Self, Self, FN);
  FWritingTools.EditCustomPrompt := EditCustomPrompt;

  EditCustomPrompt.OnKeyPress := FWritingTools.EditCustomPromptKeyPress;
  FWritingTools.ButtonAccept  := ButtonAccept;
  FWritingTools.ButtonClear   := ButtonClear;
  FWritingTools.ButtonRedo    := ButtonRedo;

  Profile := FWritingTools.Profile;

  var RowsNum := (Profile.ActionNumber + 1) div 2;
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

  for var I := 0 to Profile.ActionNumber - 1 do
  begin
    var B := TButton.Create(GridActions);
    B.Parent := GridActions;
    B.Tag := I;
    B.Align := alClient;
    var C := Profile.Action[I].Name;
    if Profile.Action[I].Shortcut <> Chr(0) then
      C := C + Format(' (&%s)', [Profile.Action[I].Shortcut]);
    B.Caption := C;
    B.OnClick := ActionButtonClick;
  end;

  LabelStatus.Caption := 'loading LLMs';
  FWritingTools.Startup;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FWritingTools.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
Var
  R: TRect;
begin
  SetRect(R, 0, 0, Width, Height);
  GradientFillCanvas(Canvas,
    FWritingTools.Profile.UIConfig.Background.Color1,
    FWritingTools.Profile.UIConfig.Background.Color2, R, gdVertical);
end;

procedure TMainForm.Hide1Click(Sender: TObject);
begin
  Visible:= False;
end;

procedure TMainForm.LLMLoaded;
begin
  Enabled := True;
  ActivityIndicator.Visible := False;
end;

procedure TMainForm.LLMStateChanged(Sender: TObject; ABusy: Boolean);
begin
  ActivityIndicator.Visible := ABusy;
  if not ABusy then
    FlushAIChunks;
end;

procedure TMainForm.SetCurrentApp(AValue: string);
begin
  if AValue = '' then
     AValue := 'about:blank'
  else
    AValue := 'file://' + AValue;
  FBrowserUpdater.ResetContent(AValue);
end;

procedure TMainForm.Show1Click(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.ShowChat;
begin
  CurrentApp := FAppChat;
end;

procedure TMainForm.ShowPage(Id: Integer);
begin
  if FActivePageId = Id then
  begin
    if PageBrowser.Visible then
      FBrowserUpdater.ResetContent();
    Exit;
  end;

  FActivePageId := Id;
  case Id of
    CARD_MAIN: begin
      CardPanel.ActiveCardIndex := 0;
    end;
    CARD_RUNNING: begin
      CardPanel.ActiveCardIndex := 1;
    end
  else
    CardPanel.ActiveCardIndex := 2;
    CurrentApp := FAppChat;
  end;
end;

procedure TMainForm.ShowStatus(Msg: string);
begin
  LabelStatus.Caption := Msg;
end;

procedure TMainForm.WebAppDiff(A, B: string);
begin
  FBrowserUpdater.CallJSFun('app_diff', A, B);
end;

procedure TMainForm.WMHotKey(var Msg: TWMHotKey);
begin
  FWritingTools.UserHotkey;
end;

{ TEdgeBrowserUpdater }

procedure TEdgeBrowserUpdater.CallJSFun(const ALine: string);
begin
  if Busy then
  begin
    FPendedCall.Add(ALine);
    Exit;
  end;
  DoCallJs(ALine);
end;

procedure TEdgeBrowserUpdater.CallJSFun(const FunName, AParam: string);
var
  S: string;
begin
  S := Format('%s("%s")', [FunName, EscapeCString(AParam)]);
  CallJSFun(S);
end;

procedure TEdgeBrowserUpdater.CallJSFun(const FunName, AParam1,
  AParam2: string);
var
  S: string;
begin
  S := Format('%s("%s", "%s")', [FunName, EscapeCString(AParam1), EscapeCString(AParam2)]);
  CallJSFun(S);
end;

procedure TEdgeBrowserUpdater.CallPendingJs;
var
  S: string;
begin
  if FPendedCall.Count > 0 then
  begin
    S := FPendedCall[0];
    FPendedCall.Delete(0);
    DoCallJs(S);
  end;
end;

constructor TEdgeBrowserUpdater.Create(AOwner: TComponent; View: TEdgeBrowser; InitialURL: string);
begin
  inherited Create(AOwner);
  FPendedCall := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := TimerTimer;
  FHtml := InitialURL;
  FView := View;

  FView.OnNavigationCompleted := BrowserNavCompleted;
  FView.Navigate(InitialURL);
end;

destructor TEdgeBrowserUpdater.Destroy;
begin
  FPendedCall.Free;
  inherited;
end;

procedure TEdgeBrowserUpdater.DoCallJs(const S: string);
begin
  FView.OnExecuteScript := ScriptExecuted;
  FView.ExecuteScript(S);
end;

function TEdgeBrowserUpdater.GetBusy: Boolean;
begin
  Result := Assigned(FView.OnExecuteScript) or Assigned(FView.OnNavigationCompleted);
end;

procedure TEdgeBrowserUpdater.ResetContent(const FilePath: string);
begin
  FPendedCall.Clear;
  if FilePath = '' then
    DoSetPage(FLastFilePath)
  else
    DoSetPage(FilePath);
end;

procedure TEdgeBrowserUpdater.BrowserNavCompleted(Sender: TCustomEdgeBrowser;
  IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
begin
  FView.OnNavigationCompleted := nil;

  CallPendingJs;
end;

procedure TEdgeBrowserUpdater.ScriptExecuted(Sender: TCustomEdgeBrowser;
  AResult: HResult; const AResultObjectAsJson: string);
begin
  FView.OnExecuteScript := nil;
  CallPendingJs;
end;

procedure TEdgeBrowserUpdater.DoSetPage(const html: string);
begin
  FLastFilePath := html;
  FView.OnNavigationCompleted := BrowserNavCompleted;
  FView.Navigate(html);
end;

procedure TEdgeBrowserUpdater.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
end;

end.
