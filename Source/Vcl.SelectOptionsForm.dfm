object fmSelectOption: TfmSelectOption
  Left = 313
  Top = 231
  BorderStyle = bsDialog
  Caption = 'Select option'
  ClientHeight = 329
  ClientWidth = 292
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object paBottom: TPanel
    Left = 0
    Top = 292
    Width = 292
    Height = 37
    Align = alBottom
    TabOrder = 0
    object paButtons: TPanel
      Left = 34
      Top = 1
      Width = 171
      Height = 35
      Align = alRight
      BevelOuter = bvLowered
      TabOrder = 0
      object bbOK: TButton
        Left = 3
        Top = 3
        Width = 81
        Height = 29
        Caption = 'Select'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object bbCancel: TButton
        Left = 87
        Top = 3
        Width = 80
        Height = 29
        Cancel = True
        Caption = 'Exit'
        ModalResult = 8
        TabOrder = 1
      end
    end
    object paHelp: TPanel
      Left = 205
      Top = 1
      Width = 86
      Height = 35
      Align = alRight
      BevelOuter = bvLowered
      TabOrder = 1
      Visible = False
      object btHelp: TButton
        Left = 3
        Top = 3
        Width = 81
        Height = 29
        Caption = 'Help'
        ModalResult = 9
        TabOrder = 0
        OnClick = btHelpClick
      end
    end
  end
  object gb: TRadioGroup
    Left = 0
    Top = 0
    Width = 292
    Height = 292
    Align = alClient
    Caption = 'Options'
    Ctl3D = True
    DoubleBuffered = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    TabOrder = 1
  end
end
