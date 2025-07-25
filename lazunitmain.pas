unit LazUnitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Windows, LMessages, ActiveX,
  ActnList, StdCtrls, ComCtrls, uWVBrowser, uWVWindowParent, uWVLoader,
  uWVBrowserBase, uWVTypes, uWVEvents, uWVTypeLibrary,
  UnitTools;

type

  { TEdgeBrowserUpdater }

  TEdgeBrowserUpdater = class(TComponent)
  private
    FOnInitialized: TNotifyEvent;
    FView: TWVBrowser;
    FViewWnd: TWVWindowParent;
    FTimer: TTimer;
    FPendedCall: TStringList;
    FHtml: string;
    FLastFilePath: string;
    procedure BrowserNavCompleted(Sender: TObject; const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2NavigationCompletedEventArgs);
    procedure BrowserAfterCreated(Sender: TObject);
    procedure SetOnInitialized(AValue: TNotifyEvent);
    procedure TimerTimer(Sender: TObject);
    function GetBusy: Boolean;
    procedure InitWebView;
    procedure DoSetPage(const html: string);
    procedure ScriptDone(Sender: TObject; aErrorCode: HRESULT; const aResultObjectAsJson: wvstring; aExecutionID: integer);
    procedure DoCallJs(const S: string);
    procedure CallPendingJs;
  public
    constructor Create(AOwner: TComponent; View: TWVBrowser; ViewWnd: TWVWindowParent;
                       AOnInitialized: TNotifyEvent;
                       InitialURL: string); reintroduce; overload;

    procedure ResetContent(const FilePath: string = '');
    procedure CallJSFun(const FunName, AParam1, AParam2: string); overload;
    procedure CallJSFun(const FunName, AParam: string); overload;
    procedure CallJSFun(const ALine: string); overload;

    destructor Destroy; override;
    property Browser: TWVBrowser read FView;
    property Busy: Boolean read GetBusy;
    property OnInitialized: TNotifyEvent read FOnInitialized write SetOnInitialized;
  end;

  { TMainForm }

  TMainForm = class(TForm, IWritingToolsUI)
    About1: TMenuItem;
    ActionAbortGeneration: TAction;
    ActionHide: TAction;
    ActionList1: TActionList;
    ActivityIndicator: TProgressBar;
    ButtonRedo: TButton;
    ButtonAccept: TButton;
    ButtonClear: TButton;
    EditCustomPrompt: TEdit;
    Exit1: TMenuItem;
    LabelStatus: TLabel;
    N1: TMenuItem;
    PageRunning: TPanel;
    PageActions: TPanel;
    PageBrowser: TPanel;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Show1: TMenuItem;
    TrayIcon1: TTrayIcon;
    WVBrowser1: TWVBrowser;
    WVWindowParent1: TWVWindowParent;
    procedure About1Click(Sender: TObject);
    procedure ActionAbortGenerationExecute(Sender: TObject);
    procedure ActionHideExecute(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageActionsResize(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure WVBrowser1AcceleratorKeyPressed(Sender: TObject;
      const aController: ICoreWebView2Controller;
      const aArgs: ICoreWebView2AcceleratorKeyPressedEventArgs);
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
    procedure LayoutButtons;
    procedure ActionButtonClick(Sender: TObject);
    procedure WMHotKey(var Msg: TMessage); message WM_HOTKEY;
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
    procedure WebInited(Sender: TObject);
    property  CurrentApp: string write SetCurrentApp;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TEdgeBrowserUpdater }

procedure TEdgeBrowserUpdater.BrowserNavCompleted(Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2NavigationCompletedEventArgs);
begin
  FView.OnNavigationCompleted := nil;
  CallPendingJs;
end;

procedure TEdgeBrowserUpdater.BrowserAfterCreated(Sender: TObject);
begin
  FViewWnd.UpdateSize;
  if FHtml <> '' then
  begin
    FView.Navigate('file://' + FHtml);
    FHtml := '';
  end;
end;

procedure TEdgeBrowserUpdater.SetOnInitialized(AValue: TNotifyEvent);
begin
  if FOnInitialized = AValue then Exit;
  FOnInitialized := AValue;
end;

procedure TEdgeBrowserUpdater.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  InitWebView;
end;

function TEdgeBrowserUpdater.GetBusy: Boolean;
begin
  Result := Assigned(FView.OnNavigationCompleted) or Assigned(FView.OnExecuteScriptCompleted);
end;

procedure TEdgeBrowserUpdater.InitWebView;
begin
  if GlobalWebView2Loader.Initialized then
  begin
    FView.CreateBrowser(FViewWnd.Handle);
  end
  else
    FTimer.Enabled := True;
end;

procedure TEdgeBrowserUpdater.DoSetPage(const html: string);
begin
  FLastFilePath := html;
  FView.OnNavigationCompleted := @BrowserNavCompleted;
  FView.Navigate(html);
end;

procedure TEdgeBrowserUpdater.ScriptDone(Sender: TObject; aErrorCode: HRESULT;
  const aResultObjectAsJson: wvstring; aExecutionID: integer);
begin
  FView.OnExecuteScriptCompleted := nil;
  CallPendingJs;
end;

procedure TEdgeBrowserUpdater.DoCallJs(const S: string);
var
  w: wvstring;
begin
  FView.OnExecuteScriptCompleted := @ScriptDone;
  w := UTF8Decode(S);
  FView.ExecuteScript(w);
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

constructor TEdgeBrowserUpdater.Create(AOwner: TComponent; View: TWVBrowser;
  ViewWnd: TWVWindowParent; AOnInitialized: TNotifyEvent; InitialURL: string);
begin
  inherited Create(AOwner);

  FPendedCall := TStringList.Create;
  FView := View;
  FViewWnd := ViewWnd;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := @TimerTimer;

  FView.OnAfterCreated := @BrowserAfterCreated;
  FHtml := InitialURL;

  InitWebView;
end;

procedure TEdgeBrowserUpdater.ResetContent(const FilePath: string);
begin
  FPendedCall.Clear;
  if FilePath = '' then
    DoSetPage(FLastFilePath)
  else
    DoSetPage(FilePath);
end;

procedure TEdgeBrowserUpdater.CallJSFun(const FunName, AParam1, AParam2: string
  );
var
  S: string;
begin
  S := Format('%s("%s", "%s")', [FunName, EscapeCString(AParam1), EscapeCString(AParam2)]);
  CallJSFun(S);
end;

procedure TEdgeBrowserUpdater.CallJSFun(const FunName, AParam: string);
var
  S: string;
begin
  S := Format('%s("%s")', [FunName, EscapeCString(AParam)]);
  CallJSFun(S);
end;

procedure TEdgeBrowserUpdater.CallJSFun(const ALine: string);
begin
  if Busy then
  begin
    FPendedCall.Add(ALine);
    Exit;
  end;
  DoCallJs(ALine);
end;

destructor TEdgeBrowserUpdater.Destroy;
begin
  FPendedCall.Free;
  inherited Destroy;
end;

{ TMainForm }

procedure TMainForm.ActionAbortGenerationExecute(Sender: TObject);
begin
  FWritingTools.AbortGeneration;
end;

procedure TMainForm.ActionHideExecute(Sender: TObject);
begin
  Hide;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  FWritingTools.About;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Enabled := False;
  FActivePageId := -1;
  if GlobalWebView2Loader.InitializationError then
  begin
    ShowMessage(UTF8Encode(GlobalWebView2Loader.ErrorMessage));
    Application.Terminate;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FWritingTools.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
Var
  R: TRect;
begin
  R := TRect.Create(0, 0, Width, Height);
  Canvas.GradientFill(R,
    FWritingTools.Profile.UIConfig.Background.Color1,
    FWritingTools.Profile.UIConfig.Background.Color2, gdVertical);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Profile: TWritingProfile;
  FN: string;
  B: string;
  I: Integer;

  function CreateButton(Id: Integer; A: TWritingAction): TButton;
  var
    C: string;
  begin
    Result := TButton.Create(PageActions);
    Result.Parent := PageActions;
    Result.Tag := Id;
    C := A.Name;
    if A.Shortcut <> Chr(0) then
      C := C + Format(' (&%s)', [Profile.Action[I].Shortcut]);
    Result.Caption := C;
    Result.OnClick := @ActionButtonClick;
  end;

begin
  OnShow := nil;

  FN := ExtractFilePath(Application.ExeName) + 'profile.json';
  B := ExtractFilePath(Application.ExeName)  + 'data' + DirectorySeparator;
  if not DirectoryExists(B) then
    B := ExtractFilePath(Application.ExeName) + '../../data' + DirectorySeparator;

  FAppChat := B + 'app_chat.html';

  if ParamCount() > 1 then FN := ParamStr(1);

  if not FileExists(FN) then
  begin
    MessageDlg(Format('profile.json not found: %s', [FN]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
    Exit;
  end;

  FBrowserUpdater := TEdgeBrowserUpdater.Create(Self, WVBrowser1, WVWindowParent1, @WebInited, FAppChat);

  FWritingTools := TWritingTools.Create(Self, Self, FN);
  FWritingTools.EditCustomPrompt := EditCustomPrompt;

  EditCustomPrompt.OnKeyPress := @FWritingTools.EditCustomPromptKeyPress;
  FWritingTools.ButtonAccept  := ButtonAccept;
  FWritingTools.ButtonClear   := ButtonClear;
  FWritingTools.ButtonRedo    := ButtonRedo;

  Profile := FWritingTools.Profile;

  SetLength(FActButtons, Profile.ActionNumber);
  for I := 0 to Profile.ActionNumber - 1 do
  begin
    FActButtons[I] := CreateButton(I, Profile.Action[I]);
  end;

  LabelStatus.Caption := 'loading LLMs';
  FWritingTools.Startup;

  ShowPage(CARD_MAIN);
end;

procedure TMainForm.PageActionsResize(Sender: TObject);
begin
  LayoutButtons;
end;

procedure TMainForm.Show1Click(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.WVBrowser1AcceleratorKeyPressed(Sender: TObject;
  const aController: ICoreWebView2Controller;
  const aArgs: ICoreWebView2AcceleratorKeyPressedEventArgs);
var
  Msg: TMessage;
  Key: SYSUINT;
  LParam: SYSINT;
  KeyEventKind: COREWEBVIEW2_KEY_EVENT_KIND;
begin
  if Failed(aArgs.Get_VirtualKey(Key))
    or Failed(aArgs.Get_KeyEventLParam(LParam))
    or Failed(aArgs.Get_KeyEventKind(KeyEventKind)) then Exit;

  case KeyEventKind of
    COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN:
      Msg.Msg := WM_KEYDOWN;
    COREWEBVIEW2_KEY_EVENT_KIND_KEY_UP:
      Msg.Msg := WM_KEYUP;
    COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN:
      Msg.Msg := WM_SYSKEYDOWN;
    COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_UP:
      Msg.Msg := WM_SYSKEYUP;
  else
    Msg.Msg := 0;
  end;
  Msg.WParam := Key;
  Msg.LParam := LParam;

  if IsShortcut(TLMKey(Msg)) then
    aArgs.Set_Handled(Integer(LongBool(True)));
end;

procedure TMainForm.SetCurrentApp(AValue: string);
begin
  if AValue = '' then
     AValue := 'about:blank'
  else
    AValue := 'file://' + AValue;
  FBrowserUpdater.ResetContent(AValue);
end;

procedure TMainForm.FlushAIChunks;
begin
  if FChunkAcc = '' then Exit;
  if FIsThought then
    FBrowserUpdater.CallJSFun('ai_append_thought_chunk', FChunkAcc)
  else
    FBrowserUpdater.CallJSFun('ai_append_chunk', FChunkAcc);
  FChunkAcc := '';
end;

procedure TMainForm.LayoutButtons;
const
  GAP_V = 20;
  GAP_H = 35;
var
  RowsNum: Integer;
  W, H: Integer;
  I: Integer;
  Row: Integer;
  Col: Integer;
begin
  RowsNum := (Length(FActButtons) + 1) div 2;
  if RowsNum < 1 then Exit;

  W := (PageActions.Width - 3 * GAP_H) div 2;
  H := (PageActions.Height - (1 + RowsNum) * GAP_V) div RowsNum;

  if (W < 10) or (H < 10) then Exit;

  for I := 0 to High(FActButtons) do
  begin
    Row := I div 2;
    Col := I mod 2;
    with FActButtons[i] do
    begin
      Width := W;
      Height := H;
      Left := GAP_H + (GAP_H + W) * Col;
      Top  := GAP_V + (GAP_V + H) * Row;
    end;
  end;
end;

procedure TMainForm.ActionButtonClick(Sender: TObject);
begin
  if not (Sender is TButton) then Exit;

  FWritingTools.DoAction((Sender as TButton).Tag);
end;

procedure TMainForm.WMHotKey(var Msg: TMessage);
begin
  FWritingTools.UserHotkey;
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
      PageActions.Visible := True;
      PageRunning.Visible := False;
    //  PageBrowser.Visible := False;
      PageActions.Align := alClient;
    end;
    CARD_RUNNING: begin
      PageActions.Visible := False;
      PageRunning.Visible := True;
      PageBrowser.Visible := False;
      PageRunning.Align := alClient;
    end
  else
    PageActions.Visible := False;
    PageRunning.Visible := False;
    PageBrowser.Visible := True;
    PageBrowser.Align := alClient;
    WVWindowParent1.UpdateSize;
    CurrentApp := FAppChat;
  end;
end;

procedure TMainForm.WebAppDiff(A, B: string);
begin
  FChunkAcc := '';
  FBrowserUpdater.CallJSFun('app_diff', A, B);
end;

procedure TMainForm.AppendAIChunk(L: string);
begin
  if FIsThought then FlushAIChunks;
  FIsThought := False;
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
  FIsThought := True;
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

procedure TMainForm.ShowStatus(Msg: string);
begin
  LabelStatus.Caption := Msg;
end;

procedure TMainForm.LLMStateChanged(Sender: TObject; ABusy: Boolean);
begin
  ActivityIndicator.Visible := ABusy;
  if not ABusy then
    FlushAIChunks;
end;

procedure TMainForm.LLMLoaded;
begin
  Enabled := True;
  ActivityIndicator.Visible := False;
end;

procedure TMainForm.ShowChat;
begin
  CurrentApp := FAppChat;
end;

procedure TMainForm.ClearChatHistory;
begin
  FlushAIChunks;
  FBrowserUpdater.CallJSFun('reset()');
end;

procedure TMainForm.WebInited(Sender: TObject);
begin

end;

initialization
  GlobalWebView2Loader                := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder := UTF8Decode(GetTempDir + 'WritingToolsCache');
  GlobalWebView2Loader.StartWebView2;

end.

