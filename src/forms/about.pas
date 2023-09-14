{ ------------------------------------------------------------------------
  Copyright (C) 2023 Chiizujin.

  This file is part of DungeonMapper.

  DungeonMapper is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  DungeonMapper is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
------------------------------------------------------------------------ }
unit About;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, LCLType;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    ButtonOK: TBitBtn;
    AboutMemo: TMemo;
    PanelBottom: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

uses
  Main;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  AboutMemo.Lines.Delete(0);
  AboutMemo.Lines.Insert(0, MainForm.MakeVersionString);
end;

procedure TAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{$push}{$warn 5024 off} { Unused parameter }
begin
  if Key in [VK_RETURN, VK_ESCAPE] then
    ModalResult := mrOK;
end;
{$pop}

end.

