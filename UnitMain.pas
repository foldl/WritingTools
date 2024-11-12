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
  MarkdownProcessor, System.Actions, Vcl.ActnList, UnitTools;

type

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
    constructor Create(AOwner: TComponent; View: TEdgeBrowser; InitialURL: string = 'about:blank'); reintroduce; overload;

    procedure ClearContentAll;
    procedure SetContentAll(Content: string);

    procedure SetPage(const html: string);

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
    Card4: TCard;
    ActivityIndicator: TProgressBar;
    BrowserForShow: TEdgeBrowser;
    ActionList1: TActionList;
    ActionAbortGeneration: TAction;
    ActionHide: TAction;
    BrowserForChat: TEdgeBrowser;
    ButtonClear: TButton;
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
    FWritingTools: TWritingTools;
    FUpdaterShow: TEdgeBrowserUpdater;
    FUpdaterChat: TEdgeBrowserUpdater;
    procedure ActionButtonClick(Sender: TObject);
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure ShowPage(Id: Integer);
    procedure UpdateAIMemo(L: string);
    procedure ShowStatus(Msg: string);
    procedure LLMStateChanged(Sender: TObject; ABusy: Boolean);
    procedure LLMLoaded;
  private
    procedure ShowChat;
  public
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
  FUpdaterShow.SetPage('<html></html>');
  Hide;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Profile: TWritingProfile;
begin
  var FN := ExtractFilePath(Application.ExeName) + 'profile.json';
  if ParamCount() > 1 then FN := ParamStr(1);

  if not FileExists(FN) then
  begin
    MessageDlg(Format('profile.json not found: %s', [FN]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
    Exit;
  end;

  FUpdaterShow := TEdgeBrowserUpdater.Create(Self, BrowserForShow);
  FUpdaterChat := TEdgeBrowserUpdater.Create(Self, BrowserForChat);

  CardPanel.ActiveCardIndex := CARD_MAIN;

  FWritingTools := TWritingTools.Create(Self, Self, FN);
  FWritingTools.EditCustomPrompt := EditCustomPrompt;

  EditCustomPrompt.OnKeyPress := FWritingTools.EditCustomPromptKeyPress;
  ButtonClear.OnClick         := FWritingTools.ButtonClearClick;

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
  ButtonClear.Enabled := not ABusy;
end;

procedure TMainForm.Show1Click(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.ShowChat;
begin
  FUpdaterChat.SetPage(FWritingTools.RenderChat);
end;

procedure TMainForm.ShowPage(Id: Integer);
begin
  if CardPanel.ActiveCardIndex <> Id then
  begin
    CardPanel.ActiveCardIndex := Id;
    if Id = CARD_SHOW then
    begin
      //FUpdaterShow.CallJS('clearContentAll', '');
      FUpdaterShow.SetPage('<html></html>');
    end;
  end;

  ButtonClear.Visible := Id = CARD_CHAT;
end;

procedure TMainForm.ShowStatus(Msg: string);
begin
  LabelStatus.Caption := Msg;
end;

procedure TMainForm.UpdateAIMemo(L: string);
begin
  FUpdaterShow.SetPage(L);
end;

procedure TMainForm.WMHotKey(var Msg: TWMHotKey);
begin
  FWritingTools.UserHotkey;
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
