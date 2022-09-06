{******************************************************************************}
{                                                                              }
{       DataAwareLabeledComponents: Dataaware Edit components with Label       }
{                                                                              }
{       Copyright (c) 2021 (Ethea S.r.l.)                                      }
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
// Delphi XE6
{$IFDEF VER270}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
{$ENDIF}

// Delphi XE7
{$IFDEF VER280}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
{$ENDIF}

// Delphi XE8
{$IFDEF VER290}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
{$ENDIF}

// Delphi 10 Seattle
{$IFDEF VER300}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
{$ENDIF}

// Delphi 10.1 Berlin
{$IFDEF VER310}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
{$ENDIF}

// Delphi 10.2 Tokyo
{$IFDEF VER320}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
{$ENDIF}

// Delphi 10.3 Rio
{$IFDEF VER330}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
{$ENDIF}

// Delphi 10.4 Sydney
{$IFDEF VER340}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
{$ENDIF}

// Delphi 11.0 Alexandria
{$IFDEF VER350}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
  {$DEFINE D11_0+}
{$ENDIF}

//GDI+ support available from DXE6
{$IFDEF DXE4+}
  {$Define GDI+}
{$ENDIF}

//StoreBitmap option introduced in Delphi 10.3 Rio
{$IFDEF D10_3+}
  {$Define HasStoreBitmapProperty}
{$ENDIF}

{$IFDEF D10_3+}
  {$Define HiDPISupport}
{$ENDIF}
