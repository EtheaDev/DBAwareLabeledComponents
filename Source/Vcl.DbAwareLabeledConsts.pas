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
unit Vcl.DbAwareLabeledConsts;

interface

const
  INTEGER_FORMAT  = '###0';
  CURRENCY_FORMAT = '#,##0.00';
  FLOAT_FORMAT = '#,##0.00';
  TIME_MASK = '!99:99:99;1; ';
  DATE_MASK = '!99/99/9999;1; ';
  DATE_TIME_HH_MM_MASK = '!99/99/9999 99:99;1; ';
  DATE_TIME_HH_MM_SS_MASK = '!99/99/9999 99:99:99;1; ';
  TIME_HH_MM_MASK = '!99:99;1; ';
  COLOR_MASK = '>aaaaaa;1; ';
  STYLE_WINDOWS = 'Windows';

resourcestring
  ERR_NO_CHANGE_DIR = 'Warning: is not possible to save data in a different folder';
  ERR_COPY_FILE = 'Cannot copy file "%s" as "%s"';
  ERR_NO_EDITING_DATASET = 'Cannot edit field if you aren''t in edit state';
  LOOKUP_CAPTION = 'Lookup reference';
  CLEAR_CAPTION = 'Clear reference';
  ERR_MAX_LENGTH = 'MaxLength property accepts only positive values';
  ERR_NUM_VALUES = 'Cannot set NumberValues without elements';
  EXT_FILES_FILTER = '%s file types|*.%s';
  ALL_FILES_FILTER = 'All files|*.*';
  ALL_IMAGES_FILTER = 'All images files (%s)|%s';
  ALL_DOCS_FILTER = 'All documents (%s)|%s';
  DEFAULT_DIALOG_TITLE = 'Select the file to open';
  UNSUPPORTED_FILE = 'Unsupported file format';
  TIME_LABEL = 'Time:';

implementation

end.
