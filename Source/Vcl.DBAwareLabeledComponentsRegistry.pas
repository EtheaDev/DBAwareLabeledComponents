{******************************************************************************}
{                                                                              }
{       DataAwareLabeledComponents: Dataaware Edit components with Label       }
{                                                                              }
{       Copyright (c) 2021-2022 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/DBAwareLabeledComponents                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Vcl.DBAwareLabeledComponentsRegistry;

{$I 'DBAwareLabeledComponents.inc'}

interface

procedure Register;

implementation

uses
  System.Classes
  , Vcl.LabeledComCtrls
  , Vcl.LabeledCtrls
  , Vcl.LabeledExtCtrls
  , Vcl.LabeledDBCtrls
  , Vcl.LabeledMask
  , Vcl.LabeledDbImage
  , Vcl.LabeledCheckLst
  , Vcl.LabeledColorGrd
  , Vcl.LabeledDBListView
  {$IFDEF D10_4+}
  , Vcl.DbNumberBox
  , Vcl.LabeledNumberBox
  {$ENDIF}
  {$IFDEF D10_1+}
  , Vcl.LabeledButtonEdit
  {$ENDIF}
  , Vcl.LabeledCurrencyEdit;

procedure Register;
begin
  {$IFDEF D10_1+}
  RegisterComponents('LabeledControls', [TLabeledButtonEdit]);
  RegisterComponents('LabeledControls', [TLabeledDBButtonEdit]);
  {$ENDIF}
  {$IFDEF D10_4+}
  RegisterComponents('DbControls', [TDBNumberBox]);
  RegisterComponents('LabeledControls', [TLabeledNumberBox]);
  RegisterComponents('LabeledControls', [TLabeledDBNumberBox]);
  {$ENDIF}
  RegisterComponents('LabeledControls', [TLabeledColorGrid]);
  RegisterComponents('LabeledControls', [TLabeledCurrencyEdit]);
  RegisterComponents('LabeledControls', [TLabeledDBCurrencyEdit]);
  RegisterComponents('LabeledControls', [TLabeledSpinEdit]);
  RegisterComponents('LabeledControls', [TLabeledRichEdit]);
  RegisterComponents('LabeledControls', [TLabeledCheckListBox]);
  RegisterComponents('LabeledControls', [TLabeledEditEx]);
  RegisterComponents('LabeledControls', [TLabeledComboBox]);
  RegisterComponents('LabeledControls', [TLabeledListBox]);
  RegisterComponents('LabeledControls', [TLabeledMemo]);
  RegisterComponents('LabeledControls', [TLabeledDBEdit]);
  RegisterComponents('LabeledControls', [TLabeledDBLabel]);
  RegisterComponents('LabeledControls', [TLabeledDBComboBox]);
  RegisterComponents('LabeledControls', [TLabeledDBListBox]);
  RegisterComponents('LabeledControls', [TLabeledDBMemo]);
  RegisterComponents('LabeledControls', [TLabeledDBImage]);
  RegisterComponents('LabeledControls', [TLabeledDBLookupListBox]);
  RegisterComponents('LabeledControls', [TLabeledDBLookupComboBox]);
  RegisterComponents('LabeledControls', [TLabeledDbGrid]);
  RegisterComponents('LabeledControls', [TLabeledDBRichEdit]);
  RegisterComponents('LabeledControls', [TLabeledDBCtrlGrid]);
  RegisterComponents('LabeledControls', [TLabeledDBListView]);
  RegisterComponents('LabeledControls', [TLabeledMaskEdit]);
  RegisterComponents('LabeledControls', [TLabeledRadioGroup]);
  RegisterComponents('LabeledControls', [TLabeledImage]);
  RegisterComponents('LabeledControls', [TLabeledColorBox]);
end;

end.
