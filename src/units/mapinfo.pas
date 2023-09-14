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
unit MapInfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  Generics.Collections;

type
  TTilePart = (tpMiddle, tpTop, tpBottom, tpLeft, tpRight);

  TTileReference = record
    X, Y: Integer;
    TilePart: TTilePart;
  end;

  TMapTile = class
    public
      X, Y: Integer;
      MainColor, TopColor, BottomColor, LeftColor, RightColor: TColor;
      IsTextOnly: Boolean;
      Text: String;

      IsSelected: Boolean;

      constructor Create;
      procedure CopyFrom(Source: TMapTile);
      function GetKey: String;
  end;

  TMapTileMap = specialize TDictionary<String, TMapTile>;

  TMapTileList = specialize TList<TMapTile>;

implementation

constructor TMapTile.Create;
begin
  TopColor := -1;
  BottomColor := -1;
  LeftColor := -1;
  RightColor := -1;
end;

procedure TMapTile.CopyFrom(Source: TMapTile);
begin
  X := Source.X;
  Y := Source.Y;
  MainColor:= Source.MainColor;
  TopColor := Source.TopColor;
  BottomColor := Source.BottomColor;
  LeftColor := Source.LeftColor;
  RightColor := Source.RightColor;
  IsTextOnly := Source.IsTextOnly;
  Text := Source.Text;
  IsSelected := Source.IsSelected;
end;

function TMapTile.GetKey: String;
begin
  Result := Format('%d,%d', [X, Y]);
end;

end.

