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
unit Utils;

{$mode objfpc}{$H+}

interface

function ContainsInvalidCharacters(TestString, InvalidCharacters: String): Boolean;
function AddSpacesToString(SourceString: String): String;

implementation

uses
  SysUtils;

function ContainsInvalidCharacters(TestString, InvalidCharacters: String): Boolean;

var C: String;

begin
  for C in TestString do
  begin
    if Pos(C, InvalidCharacters) <> 0 then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function AddSpacesToString(SourceString: String): String;

var
  NewString, C: String;

begin
  NewString := '';

  for C in SourceString do
    NewString += C + ' ';

  Result := Trim(NewString);
end;

end.
