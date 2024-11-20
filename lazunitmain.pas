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
    FView: TWVBrowser;
    FViewWnd: TWVWindowParent;
    FTimer: TTimer;
    FHtml: string;
    FHtmlPending: Boolean;
    procedure BrowserNavCompleted(Sender: TObject; const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2NavigationCompletedEventArgs);
    procedure BrowserAfterCreated(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    function GetBusy: Boolean;
    procedure InitWebView;
    procedure DoSetPage(const html: string);
  public
    constructor Create(AOwner: TComponent; View: TWVBrowser; ViewWnd: TWVWindowParent; InitialURL: string = 'about:blank'); reintroduce; overload;

    procedure ClearContentAll;
    procedure SetPage(const html: string);

    destructor Destroy; override;
    property Browser: TWVBrowser read FView;
    property Busy: Boolean read GetBusy;
  end;

  { TMainForm }

  TMainForm = class(TForm, IWritingToolsUI)
    About1: TMenuItem;
    ActionAbortGeneration: TAction;
    ActionHide: TAction;
    ActionList1: TActionList;
    ActivityIndicator: TProgressBar;
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
    FWritingTools: TWritingTools;
    FBrowserUpdater: TEdgeBrowserUpdater;
    FActButtons: array of TButton;
    FActivePageId: Integer;
    procedure LayoutButtons;
    procedure ActionButtonClick(Sender: TObject);
    procedure WMHotKey(var Msg: TMessage); message WM_HOTKEY;
    procedure ShowPage(Id: Integer);
    procedure UpdateAIMemo(L: string);
    procedure ShowStatus(Msg: string);
    procedure LLMStateChanged(Sender: TObject; ABusy: Boolean);
    procedure LLMLoaded;
    procedure ShowChat;
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

  if FHtmlPending then
  begin
    FHtmlPending := False;
    DoSetPage(FHtml);
    Exit;
  end;
end;

procedure TEdgeBrowserUpdater.BrowserAfterCreated(Sender: TObject);
begin
  FViewWnd.UpdateSize;
  if FHtmlPending then
  begin
    FHtmlPending := False;
    DoSetPage(FHtml);
  end;
end;

procedure TEdgeBrowserUpdater.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  InitWebView;
end;

function TEdgeBrowserUpdater.GetBusy: Boolean;
begin
  Result := Assigned(FView.OnNavigationCompleted);
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
  FView.OnNavigationCompleted := @BrowserNavCompleted;
  FView.NavigateToString(UnicodeString(html));
end;

constructor TEdgeBrowserUpdater.Create(AOwner: TComponent; View: TWVBrowser;
  ViewWnd: TWVWindowParent; InitialURL: string);
begin
  inherited Create(AOwner);

  FView := View;
  FViewWnd := ViewWnd;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := @TimerTimer;

  FView.OnAfterCreated := @BrowserAfterCreated;

  FHtml := InitialURL;
  FHtmlPending := True;

  InitWebView;
end;

procedure TEdgeBrowserUpdater.ClearContentAll;
begin
  SetPage('<html></html>');
end;

procedure TEdgeBrowserUpdater.SetPage(const html: string);
begin
  if Assigned(FView.OnNavigationCompleted) then
  begin
    FHtmlPending := True;
    FHtml := html;
    Exit;
  end;

  DoSetPage(html);
end;

destructor TEdgeBrowserUpdater.Destroy;
begin
  inherited Destroy;
end;

{ TMainForm }

procedure TMainForm.ActionAbortGenerationExecute(Sender: TObject);
begin
  FWritingTools.AbortGeneration;
end;

procedure TMainForm.ActionHideExecute(Sender: TObject);
begin
  FBrowserUpdater.SetPage('<html></html>');
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
  if ParamCount() > 1 then FN := ParamStr(1);

  if not FileExists(FN) then
  begin
    MessageDlg(Format('profile.json not found: %s', [FN]), TMsgDlgType.mtError, [mbOK], -1);
    Application.Terminate;
    Exit;
  end;

  FBrowserUpdater := TEdgeBrowserUpdater.Create(Self, WVBrowser1, WVWindowParent1, '');

  FWritingTools := TWritingTools.Create(Self, Self, FN);
  FWritingTools.EditCustomPrompt := EditCustomPrompt;

  EditCustomPrompt.OnKeyPress := @FWritingTools.EditCustomPromptKeyPress;
  ButtonClear.OnClick         := @FWritingTools.ButtonClearClick;

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
  if FActivePageId = Id then Exit;
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
  end;
  ButtonClear.Visible := FActivePageId = CARD_CHAT;
end;

procedure TMainForm.UpdateAIMemo(L: string);
begin
  FBrowserUpdater.SetPage(L);
end;

procedure TMainForm.ShowStatus(Msg: string);
begin
  LabelStatus.Caption := Msg;
end;

procedure TMainForm.LLMStateChanged(Sender: TObject; ABusy: Boolean);
begin
  ActivityIndicator.Visible := ABusy;
  ButtonClear.Visible := (not ABusy) and (FActivePageId = CARD_CHAT);
end;

procedure TMainForm.LLMLoaded;
begin
  Enabled := True;
  ActivityIndicator.Visible := False;
end;

procedure TMainForm.ShowChat;
begin
  FBrowserUpdater.SetPage(FWritingTools.RenderChat);
end;

initialization
  GlobalWebView2Loader                := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder := UTF8Decode(GetTempDir + 'WritingToolsCache');
  GlobalWebView2Loader.StartWebView2;

end.

