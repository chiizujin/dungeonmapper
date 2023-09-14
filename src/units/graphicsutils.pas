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
unit GraphicsUtils;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, Graphics, FPCanvas;

function ColorToHexString(SourceColor: TColor): String;
function HexStringToColor(HexString: String): TColor;
procedure CanvasRectangle(Canvas: TCanvas; X1, Y1, X2, Y2: Integer);
procedure CanvasHLine(Canvas: TCanvas; X1, Y, X2: Integer);
procedure CanvasVLine(Canvas: TCanvas; X, Y1, Y2: Integer);
function IsPointInRectangle(PointX, PointY, RectX1, RectY1, RectX2, RectY2: Integer): Boolean;
function IsRectangleInRectangle(Rect1X1, Rect1Y1, Rect1X2,
  Rect1Y2, Rect2X1, Rect2Y1, Rect2X2, Rect2Y2: Integer):Boolean;
function DoRectanglesOverlap(Rect1X1, Rect1Y1, Rect1X2,
  Rect1Y2, Rect2X1, Rect2Y1, Rect2X2, Rect2Y2: Integer): Boolean;

implementation

function ColorToHexString(SourceColor: TColor): String;
begin
  if SourceColor = -1 then
    Result := ''
  else
    Result := IntToHex(Red(SourceColor), 2)
      + IntToHex(Green(SourceColor), 2)
      + IntToHex(Blue(SourceColor), 2);
end;

function HexStringToColor(HexString: String): TColor;

var
  RedPart, GreenPart, BluePart: Integer;

begin
  if HexString = '' then
  begin
    Result := -1;
    Exit;
  end;

  try
    RedPart := StrToInt('0x' + Copy(HexString, 1, 2));
    BluePart := StrToInt('0x' + Copy(HexString, 3, 2));
    GreenPart := StrToInt('0x' + Copy(HexString, 5, 2));

    Result := RGBToColor(RedPart, BluePart, GreenPart);
  except
    on E: EConvertError do
      Result := clBlack;
  end;
end;

{ TCanvas.Line() and TCanvas.Rectangle() do not draw all the way to X2 and Y2 in some cases.
  Rectangles always seem to draw to one pixel less than requested and all horizontal or vertical
  lines do the same. Other Line() calls may draw the full length, fall even more than one pixel
  short or draw fully in one dimension but not the other (shallow angles).

  It's not known if this is Lazarus/FreePascal-specific, platform-specific or even toolkit-specific,
  but a similar problem was found on the Lazarus forum which makes reference to a legacy Delphi 1
  bug: https://forum.lazarus.freepascal.org/index.php?topic=47858.0

  Since rectangles and horizontal/vertical lines seem to be consistently incorrect and are common
  uses, CanvasRectangle(), CanvasHLine() and CanvasVLine() exist to adjust the coordinate
  arguments to draw as requested so that the adjustments do not constantly need to be made in calls
  to Line() and Rectangle() }

procedure CanvasRectangle(Canvas: TCanvas; X1, Y1, X2, Y2: Integer);
begin
  Canvas.Rectangle(X1, Y1, X2 + 1, Y2 + 1);
end;

procedure CanvasHLine(Canvas: TCanvas; X1, Y, X2: Integer);
begin
  Canvas.Line(X1, Y, X2 + 1, Y);
end;

procedure CanvasVLine(Canvas: TCanvas; X, Y1, Y2: Integer);
begin
  Canvas.Line(X, Y1, X, Y2 + 1);
end;

function IsPointInRectangle(PointX, PointY, RectX1, RectY1, RectX2, RectY2: Integer): Boolean;
begin
  Result := (PointX >= RectX1) and (PointX <= RectX2)
    and (PointY >= RectY1) and (PointY <= RectY2);
end;

function IsRectangleInRectangle(Rect1X1, Rect1Y1, Rect1X2,
  Rect1Y2, Rect2X1, Rect2Y1, Rect2X2, Rect2Y2: Integer):Boolean;
begin
  Result := (Rect1X1 >= Rect2X1) and (Rect1X2 <= Rect2X2)
    and (Rect1Y1 >= Rect2Y1) and (Rect1Y2 <= Rect2Y2);
end;

function DoRectanglesOverlap(Rect1X1, Rect1Y1, Rect1X2,
  Rect1Y2, Rect2X1, Rect2Y1, Rect2X2, Rect2Y2: Integer): Boolean;
begin
  Result := not ((Rect1X1 > Rect2X2) or (Rect1X2 < Rect2X1)
    or (Rect1Y1 > Rect2Y2) or (Rect1Y2 < Rect2Y1));
end;

end.
