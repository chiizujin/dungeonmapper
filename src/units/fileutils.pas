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
unit FileUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

function GetFileNames(FilePath, FileSpec: String;
  IncludeDirectories: Boolean = False;
  DirectoriesOnly: Boolean = False): TStringList;

implementation

function GetFileNames(FilePath, FileSpec: String;
  IncludeDirectories: Boolean = False;
  DirectoriesOnly: Boolean = False): TStringList;

var
  FileInfo: TSearchRec;
  FileNames: TStringList;
  IsDirectory: Boolean;

begin
  FilePath := Trim(FilePath);
  if FilePath <> '' then
    FilePath := IncludeTrailingPathDelimiter(FilePath);

  FileNames := TStringList.Create;
  if FindFirst(FilePath + FileSpec, faAnyFile, FileInfo) = 0 then
    repeat
      with FileInfo do
      begin
        if (Name = '.') or (name = '..') then
          Continue;

        IsDirectory := (Attr and faDirectory) = faDirectory;

        if (IsDirectory) and (not IncludeDirectories) then
          Continue;

        if (not IsDirectory) and (DirectoriesOnly) then
          Continue;

        FileNames.Add(FileInfo.Name);
      end;
    until FindNext(FileInfo) <> 0;

  Result := FileNames;
end;

end.
