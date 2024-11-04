object MainForm: TMainForm
  Left = 947
  Top = 436
  BorderIcons = [biSystemMenu]
  Caption = 'Writing Tools'
  ClientHeight = 300
  ClientWidth = 480
  Color = clBtnFace
  TransparentColorValue = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnPaint = FormPaint
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 265
    Width = 480
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    ExplicitTop = 257
    ExplicitWidth = 478
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 480
      Height = 35
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 478
      object LabelStatus: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 474
        Height = 21
        Align = alClient
        Caption = 'LabelStatus'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 60
        ExplicitHeight = 15
      end
      object ActivityIndicator: TProgressBar
        AlignWithMargins = True
        Left = 3
        Top = 30
        Width = 474
        Height = 2
        Align = alBottom
        Smooth = True
        Style = pbstMarquee
        TabOrder = 0
        ExplicitWidth = 472
      end
    end
  end
  object EditCustomPrompt: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 474
    Height = 23
    Align = alTop
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 1
    TextHint = '(Custom command)'
    OnKeyUp = EditCustomPromptKeyUp
    ExplicitWidth = 472
  end
  object CardPanel: TCardPanel
    AlignWithMargins = True
    Left = 3
    Top = 32
    Width = 474
    Height = 230
    Align = alClient
    ActiveCard = Card1
    BevelOuter = bvNone
    Caption = 'CardPanel'
    TabOrder = 2
    ExplicitWidth = 472
    ExplicitHeight = 222
    object Card1: TCard
      Left = 0
      Top = 0
      Width = 474
      Height = 230
      Caption = 'Card1'
      CardIndex = 0
      TabOrder = 0
      ExplicitWidth = 472
      ExplicitHeight = 222
      object GridActions: TGridPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 468
        Height = 224
        Align = alClient
        BevelOuter = bvNone
        Caption = 'GridActions'
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <>
        Padding.Left = 15
        Padding.Top = 5
        Padding.Right = 15
        Padding.Bottom = 5
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 466
        ExplicitHeight = 216
      end
    end
    object Card2: TCard
      Left = 0
      Top = 0
      Width = 474
      Height = 230
      Caption = 'Press Ctrl+C to abort'
      CardIndex = 1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ShowCaption = True
      TabOrder = 1
    end
    object Card3: TCard
      Left = 0
      Top = 0
      Width = 474
      Height = 230
      Caption = 'Card3'
      CardIndex = 2
      TabOrder = 2
      object ScrollContainer: TScrollBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 468
        Height = 188
        VertScrollBar.Smooth = True
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        UseWheelForScrolling = True
      end
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 197
        Width = 468
        Height = 30
        Align = alBottom
        BevelOuter = bvNone
        Caption = 'Panel1'
        ShowCaption = False
        TabOrder = 1
        object ButtonClear: TButton
          Left = 393
          Top = 0
          Width = 75
          Height = 30
          Align = alRight
          Caption = 'Clear'
          TabOrder = 0
          OnClick = ButtonClearClick
        end
      end
    end
    object Card4: TCard
      Left = 0
      Top = 0
      Width = 474
      Height = 230
      Caption = 'Card4'
      CardIndex = 3
      TabOrder = 3
      object MemoForShow: TMemo
        Left = 0
        Top = 0
        Width = 474
        Height = 230
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object TrayIcon1: TTrayIcon
    PopupMenu = PopupMenu1
    Visible = True
    Left = 32
    Top = 88
  end
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 149
    object Show1: TMenuItem
      Caption = 'Show'
      OnClick = Show1Click
    end
    object About1: TMenuItem
      Caption = 'About ...'
      OnClick = About1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = Exit1Click
    end
  end
end
