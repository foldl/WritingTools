program WritingTools;

{$IF CompilerVersion >= 21.0}

{$WEAKLINKRTTI ON}

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

{$IFEND}

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {MainForm},
  libchatllm in 'libchatllm.pas',
  Vcl.Themes,
  Vcl.Styles,
  superobject in 'superobject.pas',
  MarkdownCommonMark in 'markdown\MarkdownCommonMark.pas',
  MarkdownDaringFireball in 'markdown\MarkdownDaringFireball.pas',
  MarkdownHTMLEntities in 'markdown\MarkdownHTMLEntities.pas',
  MarkdownProcessor in 'markdown\MarkdownProcessor.pas',
  MarkdownUnicodeUtils in 'markdown\MarkdownUnicodeUtils.pas',
  UnitTools in 'UnitTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Writing Tools';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
