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
unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  ComCtrls, StdCtrls, Types, StrUtils, LCLType, Buttons,
  MapInfo, Utils, GraphicsUtils, FileUtils, About;

const
  DefaultGridSize = 50;
  ClickMovementTolerance = 3; { Maximum pixel movement to be considered a click rather than a drag }
  TileEdgeClickFactor = 0.2; { Maximum amount of tile from edge to be considered an edge click }
  UndoLevels = 10;

  DefaultMapColor = clWhite;
  DefaultGridColor = clSilver;
  DefaultLineColor = clBlack;
  DefaultSelectionColor = clBlack;
  DefaultTextColor = clBlack;
  DefaultFontScale = 0.4;

  DataDirectoryName = '.dungeonmapper';
  MapFileExtension = '.dm';
  InvalidNameChars = '<>:"/\|?*';

  ApplicationName = 'DungeonMapper';
  VersionNumber = 'v1.0.0';
  FileDelimiter = '|';

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonAbout: TSpeedButton;
    ColorButtonLeft: TColorButton;
    ColorButtonRight: TColorButton;
    ComboGroups: TComboBox;
    ComboMaps: TComboBox;
    EditText: TEdit;
    LabelGridSize: TLabel;
    LabelText: TLabel;
    LabelGroup: TLabel;
    LabelMap: TLabel;
    LabelPosition: TLabel;
    PanelLine: TPanel;
    ButtonCreateGroup: TSpeedButton;
    ButtonRenameGroup: TSpeedButton;
    ButtonDeleteGroup: TSpeedButton;
    ButtonCreateMap: TSpeedButton;
    ButtonRenameMap: TSpeedButton;
    ButtonDeleteMap: TSpeedButton;
    ButtonResetMap: TSpeedButton;
    ToolBar: TPanel;
    SpinGridSize: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCreateGroupClick(Sender: TObject);
    procedure ButtonRenameGroupClick(Sender: TObject);
    procedure ButtonDeleteGroupClick(Sender: TObject);
    procedure ButtonCreateMapClick(Sender: TObject);
    procedure ButtonRenameMapClick(Sender: TObject);
    procedure ButtonDeleteMapClick(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure SpinGridSizeChange(Sender: TObject);
    procedure ComboGroupsChange(Sender: TObject);
    procedure ComboGroupsDropDown(Sender: TObject);
    procedure ComboMapsChange(Sender: TObject);
    procedure ComboMapsDropDown(Sender: TObject);
    procedure ButtonResetMapClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    function MakeVersionString: string;

  private
    DataPath: string;
    LastMouseX, LastMouseY, MapX, MapY, MouseDownX, MouseDownY,
    BoundingBoxX, BoundingBoxY, UndoListSize, UndoPosition, WindowTop,
    WindowLeft, WindowWidth, WindowHeight: integer;
    IsLeftMouseDown, IsRightMouseDown, IsDragged: boolean;
    MapTiles: array[0..UndoLevels] of TMapTileMap;
    CopiedTiles: TMapTileMap;
    CentreXTile, CentreYTile: real;

    procedure Setup;
    procedure LoadSettings;
    procedure EnableGroupButtons;
    procedure EnableMapButtons;
    procedure ResetMapPosition;
    procedure SetDataPath;
    procedure ErrorMessage(Message: string; SubMessage: string = '');
    procedure UpdatePositionLabel;
    procedure SelectTiles;
    procedure CopySelection;
    procedure CutSelection(CopyFirst: boolean);
    procedure PasteSelection;
    procedure ClearSelection;
    procedure CalculateTileAtCentre;
    procedure CentreMap;
    procedure DrawGrid(Size: integer);
    procedure DrawTile(MapTile: TMapTile; Size: integer);
    procedure DrawTileSelection(MapTile: TMapTile; Size: integer);
    function GetClickedTileReference(X, Y, Size: integer): TTileReference;
    procedure ProcessClick(Button: TMouseButton; IsShift, IsCtrl, IsAlt: boolean;
      TileRef: TTileReference);
    function IsMapLoaded: boolean;
    procedure SaveUndoState;
    procedure CopyUntoTiles(Source, Dest: TMapTileMap);
    procedure UndoMap;
    procedure RedoMap;
    procedure ClearMap;
    procedure ClearTiles(Map: TMapTileMap);
    procedure SetTextInput(Key: string);
    procedure SetButtonColor(Button: TMouseButton; TileRef: TTileReference; Key: string);
    procedure ClearTileText(Key: string);
    procedure SetTileText(TileText: string; TileRef: TTileReference;
      Key: string; IsTextOnly: boolean);
    procedure ClearTile(TileRef: TTileReference; Key: string);
    procedure SetTile(Button: TMouseButton; TileRef: TTileReference; Key: string);
    procedure ShiftTiles(Direction: word);
    procedure SaveMap;
    procedure LoadMap;
    procedure ReadGroupNames;
    procedure ReadMapNames;
    procedure CreateGroup;
    procedure RenameGroup;
    procedure DeleteGroup;
    procedure CreateMap;
    procedure RenameMap;
    procedure DeleteMap;
    procedure Shutdown;
    procedure SaveSettings;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

// -------------------------------------------------------------------------------------------------
//  EVENT METHODS
// -------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  KeyPreview := True;
  Caption := MakeVersionString;
  Setup;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and (ssAlt in Shift) then
  begin
    ShiftTiles(Key);
    Key := 0;
    Invalidate;
    Exit;
  end;

  if (Key = VK_C) and (ssAlt in Shift) then
  begin
    CopySelection();
    Exit;
  end;

  if (Key = VK_X) and (ssAlt in Shift) then
  begin
    CutSelection(not (ssShift in Shift));
    Invalidate;
    Exit;
  end;

  if (Key = VK_V) and (ssAlt in Shift) then
  begin
    PasteSelection();
    Invalidate;
    Exit;
  end;

  if Key = VK_Z then
  begin
    if (ssAlt in Shift) and (ssShift in Shift) then
      RedoMap
    else if ssAlt in Shift then
      UndoMap;
    Invalidate;
    Exit;
  end;

  if Key = VK_ESCAPE then
  begin
    ClearSelection;
    Invalidate;
    Exit;
  end;
end;

procedure TMainForm.ButtonCreateGroupClick(Sender: TObject);
begin
  CreateGroup;
end;

procedure TMainForm.ButtonRenameGroupClick(Sender: TObject);
begin
  RenameGroup;
end;

procedure TMainForm.ButtonDeleteGroupClick(Sender: TObject);
begin
  DeleteGroup;
end;

procedure TMainForm.ButtonCreateMapClick(Sender: TObject);
begin
  CreateMap;
end;

procedure TMainForm.ButtonRenameMapClick(Sender: TObject);
begin
  RenameMap;
end;

procedure TMainForm.ButtonDeleteMapClick(Sender: TObject);
begin
  DeleteMap;
end;

procedure TMainForm.ButtonAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.SpinGridSizeChange(Sender: TObject);
begin
  CentreMap;
  UpdatePositionLabel;
  Invalidate;
end;

procedure TMainForm.ComboGroupsChange(Sender: TObject);
begin
  ClearMap;
  ReadMapNames;
  EnableGroupButtons;
  EnableMapButtons;
  Invalidate;
end;

procedure TMainForm.ComboGroupsDropDown(Sender: TObject);
begin
  SaveMap; { To save the grid size and position }
end;

procedure TMainForm.ComboMapsChange(Sender: TObject);
begin
  LoadMap;
  EnableMapButtons;
end;

procedure TMainForm.ComboMapsDropDown(Sender: TObject);
begin
  SaveMap; { To save the grid size and position }
end;

procedure TMainForm.ButtonResetMapClick(Sender: TObject);
begin
  ResetMapPosition;
end;

procedure TMainForm.FormChangeBounds(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    WindowTop := Top;
    WindowLeft := Left;
    WindowWidth := Width;
    WindowHeight := Height;
  end;

  CalculateTileAtCentre;
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    IsLeftMouseDown := True;
    if not (ssShift in Shift) then
      ClearSelection;
  end
  else if Button = mbRight then
    IsRightMouseDown := True;

  IsDragged := False;
  MouseDownX := X;
  MouseDownY := Y;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);

var
  DeltaX, DeltaY: integer;

{$push}{$warn 5024 off} { Unused parameter }
begin
  LastMouseX := X;
  LastMouseY := Y;

  DeltaX := X - MouseDownX;
  DeltaY := Y - MouseDownY;

  if IsLeftMouseDown then
  begin
    if (DeltaX > ClickMovementTolerance) or (DeltaX < -ClickMovementTolerance) or
      (DeltaY > ClickMovementTolerance) or (DeltaY < -ClickMovementTolerance) then
    begin
      BoundingBoxX := X;
      BoundingBoxY := Y;
      IsDragged := True;
      Invalidate;
    end;
  end
  else if IsRightMouseDown then
  begin
    MapX := MapX - DeltaX;
    MapY := MapY - DeltaY;
    MouseDownX := X;
    MouseDownY := Y;

    CalculateTileAtCentre;
    UpdatePositionLabel;
    IsDragged := True;
    Invalidate;
  end;
end;
{$pop}

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (X > MouseDownX - ClickMovementTolerance) and
    (X < MouseDownX + ClickMovementTolerance) and (Y > MouseDownY -
    ClickMovementTolerance) and (Y < MouseDownY + ClickMovementTolerance) then
  begin
    if Button = mbLeft then
      { Left clicks are handled here instead of in OnClick() because we don't want two single clicks
        to be interpreted as a double click event }
      ProcessClick(Button, ssShift in Shift, ssCtrl in Shift, ssAlt in Shift,
        GetClickedTileReference(X, Y, SpinGridSize.Value))
    else if (Button = mbRight) and (not IsDragged) then
      ProcessClick(Button, ssShift in Shift, ssCtrl in Shift, ssAlt in Shift,
        GetClickedTileReference(X, Y, SpinGridSize.Value));
  end;

  if Button = mbRight then
    IsRightMouseDown := False;

  if Button = mbLeft then
  begin
    if IsDragged then
      SelectTiles;
    IsLeftMouseDown := False;
    BoundingBoxX := -1;
    BoundingBoxY := -1;
    Invalidate;
  end;
end;

procedure TMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
{$push}{$warn 5024 off} { Unused parameter }
begin
  if SpinGridSize.Value >= SpinGridSize.MinValue + SpinGridSize.Increment then
    SpinGridSize.Value := SpinGridSize.Value - SpinGridSize.Increment
  else
    SpinGridSize.Value := SpinGridSize.MinValue;
end;
{$pop}

procedure TMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
{$push}{$warn 5024 off} { Unused parameter }
begin
  if SpinGridSize.Value <= SpinGridSize.MaxValue - SpinGridSize.Increment then
    SpinGridSize.Value := SpinGridSize.Value + SpinGridSize.Increment
  else
    SpinGridSize.Value := SpinGridSize.MaxValue;
end;
{$pop}

procedure TMainForm.FormPaint(Sender: TObject);

var
  Size: integer;
  MapTile: TMapTile;
  Key: string;
  SelectedKeys: TStringList;
  TextMapTiles: TMapTileList;

begin
  Canvas.Brush.Color := DefaultMapColor;
  Canvas.FillRect(0, 0, Width, Height);

  Size := SpinGridSize.Value;
  DrawGrid(Size);

  Canvas.Font.Height := Round(Size * DefaultFontScale);
  Canvas.Font.Color := DefaultTextColor;

  if MapTiles[UndoPosition].Count > 0 then
  begin
    SelectedKeys := TStringList.Create;
    TextMapTiles := TMapTileList.Create;

    { Tiles with text are drawn after tiles without text so that the text isn't drawn over by
      another tile (unless it's another text tile) }
    for MapTile in MapTiles[UndoPosition].Values do
    begin
      if MapTile.Text = '' then
        DrawTile(MapTile, Size)
      else
        TextMapTiles.Add(MapTile);

      if MapTile.IsSelected then
        SelectedKeys.Add(MapTile.GetKey);
    end;

    for MapTile in TextMapTiles do
      DrawTile(MapTile, Size);

    for Key in SelectedKeys do
    begin
      MapTiles[UndoPosition].TryGetValue(Key, MapTile);
      DrawTileSelection(MapTile, Size);
    end;

    SelectedKeys.Free;
    TextMapTiles.Free;
  end;

  if BoundingBoxX <> -1 then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Style := psDash;
    CanvasRectangle(Canvas, MouseDownX, MouseDownY, BoundingBoxX, BoundingBoxY);
    Canvas.Pen.Style := psSolid;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{$push}{$warn 5024 off} { Unused parameter }
begin
  Shutdown;
end;
{$pop}

// -------------------------------------------------------------------------------------------------
//  NON-EVENT METHODS
// -------------------------------------------------------------------------------------------------

function TMainForm.MakeVersionString: string;
begin
  Result := ApplicationName + ' ' + VersionNumber;
end;

procedure TMainForm.Setup;

var
  I: integer;

begin
  SetDataPath;

  for I := 0 to UndoLevels do
    MapTiles[I] := TMapTileMap.Create;
  CopiedTiles := TMapTileMap.Create;

  UpdatePositionLabel;

  BoundingBoxX := -1;
  BoundingBoxY := -1;

  LoadSettings;
  EnableGroupButtons;
  EnableMapButtons;
end;

procedure TMainForm.LoadSettings;

var
  FileName, GroupName, MapName, IsMaximized, FormTop, FormLeft,
  FormWidth, FormHeight: string;
  SettingsFile: TextFile;

begin
  FileName := DataPath + 'dungeonmapper.conf';
  AssignFile(SettingsFile, FileName);
  try
    ReSet(SettingsFile);
    ReadLn(SettingsFile, GroupName);
    ReadLn(SettingsFile, MapName);
    ReadLn(SettingsFile, FormTop);
    ReadLn(SettingsFile, FormLeft);
    ReadLn(SettingsFile, FormWidth);
    ReadLn(SettingsFile, FormHeight);
    ReadLn(SettingsFile, IsMaximized);
    CloseFile(SettingsFile);

    ReadGroupNames;
    { Setting the text to the last selected group name here will cause the incorrect group name to
      be populated after the control's widget is sorted when it gets created later if the index
      isn't the same after sorting. It seems that, after sorting, the text is updated to match the
      index (which doesn't change) rather than updating the index to match the text if it exists in
      the list.

      To work around this we can force the control's widget to be created using HandleNeeded(), then
      set the text.

      Related information:
      https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/37284 }
    ComboGroups.HandleNeeded;
    ComboGroups.Text := GroupName;

    Top := StrToInt(FormTop);
    Left := StrToInt(FormLeft);
    Width := StrToInt(FormWidth);
    Height := StrToInt(FormHeight);

    WindowTop := Top;
    WindowLeft := Left;
    WindowWidth := Width;
    WindowHeight := Height;

    if IsMaximized = 'Y' then
      WindowState := wsMaximized;

    if ComboGroups.Text <> '' then
    begin
      ReadMapNames;
      { Force creation before setting the text, as for ComboGroups }
      ComboMaps.HandleNeeded;
      ComboMaps.Text := MapName;
      LoadMap;
    end;
  except
    on E: EInOutError do
    begin
      { Ignore missing file }
    end;

    on E: EConvertError do
      ErrorMessage(Format('Error parsing settings file (%s):', [E.Message]), FileName);
  end;
end;

procedure TMainForm.EnableGroupButtons;

var
  Enable: boolean;

begin
  ComboGroups.Enabled := ComboGroups.Items.Count <> 0;

  Enable := ComboGroups.Text <> '';
  ButtonRenameGroup.Enabled := Enable;
  ButtonDeleteGroup.Enabled := Enable;
end;

procedure TMainForm.EnableMapButtons;

var
  Enable: boolean;

begin
  ComboMaps.Enabled := ComboMaps.Items.Count <> 0;
  ButtonCreateMap.Enabled := ComboGroups.Text <> '';

  Enable := ComboMaps.Text <> '';
  ButtonRenameMap.Enabled := Enable;
  ButtonDeleteMap.Enabled := Enable;
end;

procedure TMainForm.ResetMapPosition;
begin
  SpinGridSize.Value := DefaultGridSize;
  MapX := 0;
  MapY := 0;

  UpdatePositionLabel;
  CalculateTileAtCentre;
  Invalidate;
end;

procedure TMainForm.SetDataPath;
begin
  DataPath := IncludeTrailingPathDelimiter(ConcatPaths([GetUserDir, DataDirectoryName]));
  if not DirectoryExists(DataPath) then
    if not CreateDir(DataPath) then
      ErrorMessage('Unable to create data directory:', DataPath);
end;

procedure TMainForm.ErrorMessage(Message: string; SubMessage: string = '');
begin
  if SubMessage <> '' then
    Message += #13#13 + SubMessage;
  Application.MessageBox(PChar(Message), 'DungeonMapper Error', MB_ICONERROR);
end;

procedure TMainForm.CalculateTileAtCentre;

{ This is used to keep the same part of the map at the centre of the display after changing the
  grid size }

var
  Size: integer;
  PosX, PosY: integer;

begin
  Size := SpinGridSize.Value;

  PosX := MapX - -(Width div 2);
  PosY := MapY - -((Height - Toolbar.Height) div 2);

  CentreXTile := (PosX div Size) + ((PosX mod Size) / Size);
  CentreYTile := (PosY div Size) + ((PosY mod Size) / Size);
end;

procedure TMainForm.CentreMap;

var
  Size: integer;

begin
  Size := SpinGridSize.Value;

  MapX := -((Width div 2) - Round(CentreXTile * Size));
  MapY := -(((Height - Toolbar.Height) div 2) - Round(CentreYTile * Size));
end;

procedure TMainForm.UpdatePositionLabel;
begin
  LabelPosition.Caption := Format('Position: %d,%d', [MapX, MapY]);
end;

procedure TMainForm.SelectTiles;

var
  MapTile: TMapTile;
  RangeTop, RangeLeft, RangeBottom, RangeRight, Offset, Size, TileX, TileY: integer;

begin
  if (not IsMapLoaded) or (MapTiles[UndoPosition].Count = 0) then
    Exit;

  if MouseDownX < BoundingBoxX then
  begin
    RangeLeft := MouseDownX;
    RangeRight := BoundingBoxX;
  end
  else
  begin
    RangeLeft := BoundingBoxX;
    RangeRight := MouseDownX;
  end;

  if MouseDownY < BoundingBoxY then
  begin
    RangeTop := MouseDownY;
    RangeBottom := BoundingBoxY;
  end
  else
  begin
    RangeTop := BoundingBoxY;
    RangeBottom := MouseDownY;
  end;

  Offset := ToolBar.Height;
  Size := SpinGridSize.Value;
  for MapTile in MapTiles[UndoPosition].Values do
    with MapTile do
    begin
      TileX := (X * Size) - MapX;
      TileY := (Y * Size) - MapY + Offset;
      if DoRectanglesOverlap(TileX, TileY, TileX + Size, TileY + Size,
        RangeLeft, RangeTop, RangeRight, RangeBottom) then
        IsSelected := True;
    end;
end;

procedure TMainForm.CopySelection;

var
  MapTile, CopiedTile: TMapTile;
  TempCopiedTiles: TMapTileMap;
  CopiedFromX, CopiedFromY: integer;
  IsFirst: boolean;

begin
  if (not IsMapLoaded) or (MapTiles[UndoPosition].Count = 0) then
    Exit;

  IsFirst := True;
  TempCopiedTiles := TMapTileMap.Create;
  for MapTile in MapTiles[UndoPosition].Values do
  begin
    if MapTile.IsSelected then
    begin
      CopiedTile := TMapTile.Create;
      CopiedTile.CopyFrom(MapTile);
      CopiedTile.IsSelected := False;
      TempCopiedTiles.Add(CopiedTile.GetKey, CopiedTile);

      if IsFirst then
      begin
        CopiedFromX := MapTile.X;
        CopiedFromY := MapTile.Y;
        IsFirst := False;
      end
      else
      begin
        if MapTile.X < CopiedFromX then
          CopiedFromX := MapTile.X;
        if MapTile.Y < CopiedFromY then
          CopiedFromY := MapTile.Y;
      end;
    end;
  end;

  if TempCopiedTiles.Count = 0 then
  begin
    TempCopiedTiles.Free;
    Exit;
  end;

  { Change coordinates to be a delta from the top left most position so that tiles can be pasted
    across maps }
  for MapTile in TempCopiedTiles.Values do
  begin
    MapTile.X -= CopiedFromX;
    MapTile.Y -= CopiedFromY;
  end;

  ClearTiles(CopiedTiles);
  CopiedTiles.Free;
  CopiedTiles := TempCopiedTiles;
end;

procedure TMainForm.CutSelection(CopyFirst: boolean);

var
  MapTile: TMapTile;
  Key: string;
  RemoveKeys: TStringList;

begin
  if (not IsMapLoaded) or (MapTiles[UndoPosition].Count = 0) then
    Exit;

  if CopyFirst then
    CopySelection;

  SaveUndoState;

  RemoveKeys := TStringList.Create;
  for MapTile in MapTiles[UndoPosition].Values do
  begin
    if MapTile.IsSelected then
    begin
      Key := MapTile.GetKey;
      MapTile.Free;
      RemoveKeys.Add(Key);
    end;
  end;

  for Key in RemoveKeys do
    MapTiles[UndoPosition].Remove(Key);

  RemoveKeys.Free;
end;

procedure TMainForm.PasteSelection;

var
  MapTile, ReplacedTile, PastedTile: TMapTile;
  TileReference: TTileReference;
  Key: string;

begin
  if (not IsMapLoaded) or (MapTiles[UndoPosition].Count = 0) or
    (CopiedTiles.Count = 0) then
    Exit;

  SaveUndoState;

  TileReference := GetClickedTileReference(LastMouseX, LastMouseY, SpinGridSize.Value);

  for MapTile in CopiedTiles.Values do
  begin
    PastedTile := TMapTile.Create;
    PastedTile.CopyFrom(MapTile);
    PastedTile.X := TileReference.X + PastedTile.X;
    PastedTile.Y := TileReference.Y + PastedTile.Y;

    Key := PastedTile.GetKey;
    if MapTiles[UndoPosition].TryGetValue(Key, ReplacedTile) then
    begin
      ReplacedTile.Free;
      MapTiles[UndoPosition].Remove(Key);
    end;
    MapTiles[UndoPosition].Add(Key, PastedTile);
  end;
end;

procedure TMainForm.ClearSelection;

var
  MapTile: TMapTile;

begin
  if (IsMapLoaded) and (MapTiles[UndoPosition].Count > 0) then
    for MapTile in MapTiles[UndoPosition].Values do
      MapTile.IsSelected := False;
end;

procedure TMainForm.DrawGrid(Size: integer);

var
  GridOffset, I, GridI: integer;

begin
  Canvas.Pen.Color := DefaultGridColor;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;

  GridOffset := MapX mod Size;
  for I := 0 to (Width div Size) + 1 do
  begin
    GridI := (I * Size) - GridOffset;
    CanvasVLine(Canvas, GridI, 0, Height);
  end;

  GridOffset := MapY mod Size;
  for I := 0 to (Height div Size) + 1 do
  begin
    GridI := (I * Size) - GridOffset + Toolbar.Height;
    CanvasHLine(Canvas, 0, GridI, Width);
  end;
end;

procedure TMainForm.DrawTile(MapTile: TMapTile; Size: integer);

var
  X, Y, TileLeft, TileRight, TileTop, TileBottom, TextTop, TextLeft: integer;

begin
  X := MapTile.X;
  Y := MapTile.Y;

  TileLeft := (X * Size) - MapX;
  TileRight := TileLeft + Size;
  TileTop := (Y * Size) - MapY + ToolBar.Height;
  TileBottom := TileTop + Size;

  if (TileLeft > Width) or (TileTop > Height) or (TileRight < 0) or (TileBottom < 0) then
    Exit;

  { See comment in GraphicsUtils regarding TCanvas.Line() and TCanvas.Rectangle() }

  if MapTile.IsTextOnly then
  begin
    Canvas.Brush.Color := DefaultMapColor;
    Canvas.Pen.Color := DefaultMapColor;
    CanvasRectangle(Canvas, TileLeft + 1, TileTop + 1, TileRight - 1, TileBottom - 1);
  end
  else
  begin
    Canvas.Brush.Color := MapTile.MainColor;
    Canvas.Pen.Color := DefaultLineColor;
    CanvasRectangle(Canvas, TileLeft, TileTop, TileRight, TileBottom);

    if MapTile.TopColor <> -1 then
    begin
      Canvas.Pen.Color := MapTile.TopColor;
      CanvasHLine(Canvas, TileLeft + 1, TileTop + 1, TileRight - 1);
      CanvasHLine(Canvas, TileLeft + 2, TileTop + 2, TileRight - 2);
      CanvasHLine(Canvas, TileLeft + 3, TileTop + 3, TileRight - 3);
      CanvasHLine(Canvas, TileLeft + 4, TileTop + 4, TileRight - 4);
    end;

    if MapTile.BottomColor <> -1 then
    begin
      Canvas.Pen.Color := MapTile.BottomColor;
      CanvasHLine(Canvas, TileLeft + 1, TileBottom - 1, TileRight - 1);
      CanvasHLine(Canvas, TileLeft + 2, TileBottom - 2, TileRight - 2);
      CanvasHLine(Canvas, TileLeft + 3, TileBottom - 3, TileRight - 3);
      CanvasHLine(Canvas, TileLeft + 4, TileBottom - 4, TileRight - 4);
    end;

    if MapTile.LeftColor <> -1 then
    begin
      Canvas.Pen.Color := MapTile.LeftColor;
      CanvasVLine(Canvas, TileLeft + 1, TileTop + 1, TileBottom - 1);
      CanvasVLine(Canvas, TileLeft + 2, TileTop + 2, TileBottom - 2);
      CanvasVLine(Canvas, TileLeft + 3, TileTop + 3, TileBottom - 3);
      CanvasVLine(Canvas, TileLeft + 4, TileTop + 4, TileBottom - 4);
    end;

    if MapTile.RightColor <> -1 then
    begin
      Canvas.Pen.Color := MapTile.RightColor;
      CanvasVLine(Canvas, TileRight - 1, TileTop + 1, TileBottom - 1);
      CanvasVLine(Canvas, TileRight - 2, TileTop + 2, TileBottom - 2);
      CanvasVLine(Canvas, TileRight - 3, TileTop + 3, TileBottom - 3);
      CanvasVLine(Canvas, TileRight - 4, TileTop + 4, TileBottom - 4);
    end;
  end;

  if MapTile.Text <> '' then
  begin
    if MapTile.IsTextOnly then
    begin
      TextLeft := TileLeft + Round(Size * 0.1);
      Canvas.Brush.Color := DefaultMapColor;
      Canvas.Brush.Style := bsSolid;
    end
    else
    begin
      TextLeft := TileLeft + Round((Size / 2) - (Canvas.TextWidth(MapTile.Text) / 2));
      Canvas.Brush.Style := bsClear;
    end;

    TextTop := TileTop + Round((Size / 2) - (Canvas.TextHeight(MapTile.Text) / 2));
    Canvas.TextOut(TextLeft, TextTop, MapTile.Text);

    Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TMainForm.DrawTileSelection(MapTile: TMapTile; Size: integer);

var
  X, Y, TileLeft, TileRight, TileTop, TileBottom: integer;

begin
  X := MapTile.X;
  Y := MapTile.Y;

  TileLeft := (X * Size) - MapX;
  TileRight := TileLeft + Size;
  TileTop := (Y * Size) - MapY + ToolBar.Height;
  TileBottom := TileTop + Size;

  if (TileLeft > Width) or (TileTop > Height) or (TileRight < 0) or (TileBottom < ToolBar.Height) then
    Exit;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := DefaultGridColor;
  CanvasRectangle(Canvas, TileLeft, TileTop, TileRight, TileBottom);

  Canvas.Pen.Style := psDash;
  Canvas.Pen.Width := 2;
  Canvas.Pen.Color := DefaultSelectionColor;
  CanvasRectangle(Canvas, TileLeft, TileTop, TileRight, TileBottom);
end;

function TMainForm.GetClickedTileReference(X, Y, Size: integer): TTileReference;

var
  AdjustedX, AdjustedY, TileX, TileY, TileInnerX, TileInnerY, EdgePixels: integer;
  TileRef: TTileReference;

begin
  AdjustedX := X + MapX;
  AdjustedY := Y + MapY - Toolbar.Height;
  TileX := AdjustedX div Size;
  TileY := AdjustedY div Size;

  if AdjustedX < 0 then
    TileX := TileX - 1;
  if AdjustedY < 0 then
    TileY := TileY - 1;

  TileInnerX := AdjustedX mod Size;
  TileInnerY := AdjustedY mod Size;
  { Make inner-tile coordinates positive for negative index MapTiles }
  if TileInnerX < 0 then
    TileInnerX := Size + TileInnerX;
  if TileInnerY < 0 then
    TileInnerY := Size + TileInnerY;

  EdgePixels := Round(Size * TileEdgeClickFactor);

  if TileInnerX <= EdgePixels then
    TileRef.TilePart := tpLeft
  else if TileInnerX >= Size - EdgePixels then
    TileRef.TilePart := tpRight
  else if TileInnerY <= EdgePixels then
    TileRef.TilePart := tpTop
  else if TileInnerY >= Size - EdgePixels then
    TileRef.TilePart := tpBottom
  else
    TileRef.TilePart := tpMiddle;

  TileRef.X := TileX;
  TileRef.Y := TileY;

  Result := TileRef;
end;

procedure TMainForm.ProcessClick(Button: TMouseButton;
  IsShift, IsCtrl, IsAlt: boolean; TileRef: TTileReference);

var
  Key: string;

begin
  if not IsMapLoaded then
    Exit;

  Key := Format('%d,%d', [TileRef.X, TileRef.Y]);

  { Colour pickers }
  if IsShift and IsCtrl and IsAlt then
  begin
    if Button = mbLeft then
      ColorButtonLeft.Click
    else if Button = mbRight then
      ColorButtonRight.Click;
  end
  { Set text input }
  else if IsShift and IsAlt then
    SetTextInput(Key)
  else if IsShift then
    SetButtonColor(Button, TileRef, Key)
  { Clear tile text }
  else if IsAlt and IsCtrl then
  begin
    SaveUndoState;
    ClearTileText(Key);
    SaveMap;
    Invalidate;
  end
  { Set tile text }
  else if IsAlt then
  begin
    SaveUndoState;
    SetTileText(EditText.Text, TileRef, Key, Button = mbRight);
    SaveMap;
    Invalidate;
  end
  { Clear tile }
  else if IsCtrl then
  begin
    SaveUndoState;
    ClearTile(TileRef, Key);
    SaveMap;
    Invalidate;
  end
  { Set tile }
  else
  begin
    SaveUndoState;
    SetTile(Button, TileRef, Key);
    SaveMap;
    Invalidate;
  end;
end;

function TMainForm.IsMapLoaded: boolean;
begin
  Result := ComboMaps.Text <> '';
end;

procedure TMainForm.SaveUndoState;

var
  I: integer;

begin
  if UndoPosition > 0 then
  begin
    { Truncate any "redo" by shifting remaining "undo" up to the front and resetting the position }
    for I := UndoPosition to UndoListSize do
      CopyUntoTiles(MapTiles[I], MapTiles[I - UndoPosition]);
    UndoListSize -= UndoPosition;
    UndoPosition := 0;
  end;

  { Shift the "undo" down the list leaving the front of the list as it is, ready to be changed }
  for I := 10 downto 1 do
    CopyUntoTiles(MapTiles[I - 1], MapTiles[I]);

  if UndoListSize < 10 then
    UndoListSize += 1;
end;

procedure TMainForm.CopyUntoTiles(Source, Dest: TMapTileMap);

var
  MapTile, TileCopy: TMapTile;

begin
  ClearTiles(Dest);

  for MapTile in Source.Values do
  begin
    TileCopy := TMapTile.Create;
    TileCopy.CopyFrom(MapTile);
    Dest.Add(TileCopy.GetKey, TileCopy);
  end;
end;

procedure TMainForm.UndoMap;
begin
  if (not IsMapLoaded) or (UndoPosition = UndoListSize) then
    Exit;

  UndoPosition += 1;
  SaveMap;
end;

procedure TMainForm.RedoMap;
begin
  if (not IsMapLoaded) or (UndoPosition = 0) then
    Exit;

  UndoPosition -= 1;
  SaveMap;
end;

procedure TMainForm.ClearMap;

var
  I: integer;

begin
  for I := 0 to UndoLevels do
    ClearTiles(MapTiles[I]);

  UndoListSize := 0;
  UndoPosition := 0;
end;

procedure TMainForm.SetTextInput(Key: string);

var
  MapTile: TMapTile;

begin
  if MapTiles[UndoPosition].TryGetValue(Key, MapTile) then
  begin
    EditText.Text := MapTile.Text;
    EditText.SetFocus;
    EditText.SelStart := High(integer);
  end;
end;

procedure TMainForm.SetButtonColor(Button: TMouseButton; TileRef: TTileReference;
  Key: string);

var
  ButtonColor: TColor;
  MapTile: TMapTile;

begin
  if MapTiles[UndoPosition].TryGetValue(Key, MapTile) then
  begin
    if TileRef.TilePart = tpTop then
      ButtonColor := MapTile.TopColor
    else if TileRef.TilePart = tpBottom then
      ButtonColor := MapTile.BottomColor
    else if TileRef.TilePart = tpLeft then
      ButtonColor := MapTile.LeftColor
    else if TileRef.TilePart = tpRight then
      ButtonColor := MapTile.RightColor
    else
      ButtonColor := MapTile.MainColor;

    if ButtonColor <> -1 then
      if Button = mbLeft then
        ColorButtonLeft.ButtonColor := ButtonColor
      else
        ColorButtonRight.ButtonColor := ButtonColor;
  end;
end;

procedure TMainForm.ClearTileText(Key: string);

var
  MapTile: TMapTile;

begin
  if MapTiles[UndoPosition].TryGetValue(Key, MapTile) then
  begin
    MapTile.Text := '';
    MapTile.IsTextOnly := False;
  end;
end;

procedure TMainForm.SetTileText(TileText: string; TileRef: TTileReference;
  Key: string; IsTextOnly: boolean);

var
  MapTile: TMapTile;

begin
  if Pos(FileDelimiter, TileText) <> 0 then
  begin
    ErrorMessage('Tile text may not contain:', FileDelimiter);
    Exit;
  end;

  if not MapTiles[UndoPosition].TryGetValue(Key, MapTile) then
  begin
    MapTile := TMapTile.Create;
    MapTile.X := TileRef.X;
    MapTile.Y := TileRef.Y;
    MapTile.MainColor := DefaultMapColor;
    MapTiles[UndoPosition].Add(Key, MapTile);
  end;

  MapTile.Text := TileText;
  MapTile.IsTextOnly := IsTextOnly;
end;

procedure TMainForm.ClearTile(TileRef: TTileReference; Key: string);

var
  MapTile: TMapTile;

begin
  if MapTiles[UndoPosition].TryGetValue(Key, MapTile) then
    if TileRef.TilePart = tpTop then
      MapTile.TopColor := -1
    else if TileRef.TilePart = tpBottom then
      MapTile.BottomColor := -1
    else if TileRef.TilePart = tpLeft then
      MapTile.LeftColor := -1
    else if TileRef.TilePart = tpRight then
      MapTile.RightColor := -1
    else
    begin
      MapTile.Free;
      MapTiles[UndoPosition].Remove(Key);
    end;
end;

procedure TMainForm.SetTile(Button: TMouseButton; TileRef: TTileReference; Key: string);

var
  TileColor: TColor;
  MapTile: TMapTile;

begin
  if not MapTiles[UndoPosition].TryGetValue(Key, MapTile) then
  begin
    MapTile := TMapTile.Create;
    MapTile.X := TileRef.X;
    MapTile.Y := TileRef.Y;
    MapTile.MainColor := DefaultMapColor;
    MapTiles[UndoPosition].Add(Key, MapTile);
  end;

  if Button = mbLeft then
    TileColor := ColorButtonLeft.ButtonColor
  else
    TileColor := ColorButtonRight.ButtonColor;

  if TileRef.TilePart = tpTop then
    MapTile.TopColor := TileColor
  else if TileRef.TilePart = tpBottom then
    MapTile.BottomColor := TileColor
  else if TileRef.TilePart = tpLeft then
    MapTile.LeftColor := TileColor
  else if TileRef.TilePart = tpRight then
    MapTile.RightColor := TileColor
  else
    MapTile.MainColor := TileColor;
end;

procedure TMainForm.ShiftTiles(Direction: word);

var
  XShift, YShift: integer;
  ShiftedTiles: TMapTileMap;
  MapTile, ReplacedTile: TMapTile;
  Key: string;

begin
  if (not IsMapLoaded) or (MapTiles[UndoPosition].Count = 0) then
    Exit;

  SaveUndoState;

  XShift := 0;
  YShift := 0;
  case Direction of
    VK_UP: YShift := -1;
    VK_DOWN: YShift := 1;
    VK_LEFT: XShift := -1;
    VK_RIGHT: XShift := 1;
  end;

  ShiftedTiles := TMapTileMap.Create;

  for MapTile in MapTiles[UndoPosition].Values do
  begin
    if MapTile.IsSelected then
    begin
      Key := MapTile.GetKey;
      ShiftedTiles.Add(Key, MapTile);
    end;
  end;

  { We only clear the keys here as the actual tiles are now referenced in ShiftedTiles }
  for MapTile in ShiftedTiles.Values do
    MapTiles[UndoPosition].Remove(MapTile.GetKey);

  for MapTile in ShiftedTiles.Values do
  begin
    MapTile.X += XShift;
    MapTile.Y += YShift;
    Key := MapTile.GetKey;

    if MapTiles[UndoPosition].TryGetValue(Key, ReplacedTile) then
    begin
      ReplacedTile.Free;
      MapTiles[UndoPosition].Remove(Key);
    end;

    MapTiles[UndoPosition].Add(Key, MapTile);
  end;

  ShiftedTiles.Free;

  SaveMap;
end;

procedure TMainForm.SaveMap;

var
  MapFile: TextFile;
  MapTile: TMapTile;
  GroupName, MapName, FileName, Line, TextOnlyFlag: string;

begin
  GroupName := ComboGroups.Text;
  MapName := ComboMaps.Text;

  if MapName = '' then
    Exit;

  FileName := ConcatPaths([DataPath, GroupName, MapName + MapFileExtension]);
  AssignFile(MapFile, FileName);

  try
    ReWrite(MapFile);
    Line := Format('%d%s%d%s%d', [SpinGridSize.Value, FileDelimiter, MapX, FileDelimiter, MapY]);
    WriteLn(MapFile, Line);

    for MapTile in MapTiles[UndoPosition].Values do
    begin
      with MapTile do
      begin
        if IsTextOnly then
          TextOnlyFlag := 'Y'
        else
          TextOnlyFlag := 'N';

        Line := Format('%d%s%d%s%s%s%s%s%s%s%s%s%s%s%s%s%s',
          [X, FileDelimiter,
           Y, FileDelimiter,
           ColorToHexString(MainColor), FileDelimiter,
           ColorToHexString(TopColor), FileDelimiter,
           ColorToHexString(BottomColor), FileDelimiter,
           ColorToHexString(LeftColor), FileDelimiter,
           ColorToHexString(RightColor), FileDelimiter,
           TextOnlyFlag, FileDelimiter,
           Text]);
        WriteLn(MapFile, Line);
      end;
    end;

    CloseFile(MapFile);
  except
    on E: EInOutError do
      ErrorMessage(Format('Error writing file (%s):', [E.Message]), FileName);
  end;
end;

procedure TMainForm.ClearTiles(Map: TMapTileMap);

var
  MapTile: TMapTile;

begin
  for MapTile in Map.Values do
    MapTile.Free;
  Map.Clear;
end;

procedure TMainForm.LoadMap;

var
  MapFile: TextFile;
  GroupName, MapName, FileName, Line: string;
  Parts: TStringDynArray;
  MapTile: TMapTile;

begin
  GroupName := ComboGroups.Text;
  MapName := ComboMaps.Text;

  if MapName = '' then
    Exit;

  ClearMap;
  FileName := ConcatPaths([DataPath, GroupName, MapName + MapFileExtension]);
  AssignFile(MapFile, FileName);

  try
    ReSet(MapFile);
    ReadLn(MapFile, Line);
    Parts := SplitString(Line, FileDelimiter);
    SpinGridSize.Value := StrToInt(Parts[0]);
    MapX := StrToInt(Parts[1]);
    MapY := StrToInt(Parts[2]);

    while not EOF(MapFile) do
    begin
      ReadLn(MapFile, Line);
      Parts := SplitString(Line, FileDelimiter);
      MapTile := TMapTile.Create;
      MapTile.X := StrToInt(Parts[0]);
      MapTile.Y := StrToInt(Parts[1]);
      MapTile.MainColor := HexStringToColor(Parts[2]);
      MapTile.TopColor := HexStringToColor(Parts[3]);
      MapTile.BottomColor := HexStringToColor(Parts[4]);
      MapTile.LeftColor := HexStringToColor(Parts[5]);
      MapTile.RightColor := HexStringToColor(Parts[6]);
      if Parts[7] = 'Y' then
        MapTile.IsTextOnly := True
      else
        MapTile.IsTextOnly := False;
      MapTile.Text := Parts[8];

      MapTiles[UndoPosition].Add(Format('%d,%d', [MapTile.X, MapTile.Y]), MapTile);
    end;

    CloseFile(MapFile);
  except
    on E: EInOutError do
      ErrorMessage(Format('Error reading file (%s):', [E.Message]), FileName);
    on E: EConvertError do
      ErrorMessage(Format('Error reading file (%s):', [E.Message]), FileName);
  end;

  CalculateTileAtCentre;
  UpdatePositionLabel;
  Invalidate;
end;

procedure TMainForm.ReadGroupNames;

var
  FileNames: TStringList;
  FileName: string;

begin
  FileNames := GetFileNames(DataPath, '*', True, True);

  ComboGroups.Clear;
  for FileName in FileNames do
    ComboGroups.Items.Add(FileName);

  FileNames.Free;
end;

procedure TMainForm.ReadMapNames;

var
  MapNames: TStringList;
  PathName, MapName: string;

begin
  ComboMaps.Items.Clear;
  PathName := ConcatPaths([DataPath, ComboGroups.Text]);

  MapNames := GetFileNames(PathName, '*.dm');
  for MapName in MapNames do
    ComboMaps.Items.Add(LeftStr(MapName, Length(MapName) - 3));

  MapNames.Free;
end;

procedure TMainForm.CreateGroup;

var
  GroupName, DirName: string;

begin
  GroupName := InputBox('Create Group', 'Group name:', '');
  if GroupName = '' then
    Exit;

  if ContainsInvalidCharacters(GroupName, InvalidNameChars) then
  begin
    ErrorMessage('Group names may not contain any of:', AddSpacesToString(InvalidNameChars));
    Exit;
  end;

  DirName := ConcatPaths([DataPath, GroupName]);
  if DirectoryExists(DirName) then
  begin
    ErrorMessage('Group already exists:', GroupName);
    Exit;
  end;

  if CreateDir(DirName) then
  begin
    ReadGroupNames;
    ComboGroups.Text := GroupName;
    ComboMaps.Clear;
    EnableGroupButtons;
    EnableMapButtons;
    ClearMap;
  end
  else
    ErrorMessage('Unable to create directory:', DirName);
end;

procedure TMainForm.RenameGroup;

var
  GroupName, NewGroupName, DirName, NewDirName: string;

begin
  GroupName := ComboGroups.Text;
  NewGroupName := InputBox('Rename Group', 'New group name:', GroupName);
  if (NewGroupName = GroupName) or (NewGroupName = '') then
    Exit;

  if ContainsInvalidCharacters(NewGroupName, InvalidNameChars) then
  begin
    ErrorMessage('Group names may not contain any of:', AddSpacesToString(InvalidNameChars));
    Exit;
  end;

  DirName := ConcatPaths([DataPath, GroupName]);
  NewDirName := ConcatPaths([DataPath, NewGroupName]);
  if FileExists(NewDirName) then
  begin
    ErrorMessage('Group already exists:', NewDirName);
    Exit;
  end;

  if RenameFile(DirName, NewDirName) then
  begin
    ReadGroupNames;
    ComboGroups.Text := NewGroupName;
  end
  else
    ErrorMessage('Unable to rename group:', GroupName);
end;

procedure TMainForm.DeleteGroup;

var
  GroupName, Message: string;
  DirName: string;

begin
  if ComboMaps.Items.Count <> 0 then
  begin
    Application.MessageBox('Only empty groups may be deleted.',
      'Delete Group', MB_ICONERROR);
    Exit;
  end;

  GroupName := ComboGroups.Text;
  Message := Format('Delete the group "%s"?', [GroupName]);
  if Application.MessageBox(PChar(Message), 'Delete Group', MB_ICONQUESTION +
    MB_YESNO) <> idYes then
    Exit;

  DirName := ConcatPaths([DataPath, GroupName]);
  if RemoveDir(DirName) then
  begin
    ReadGroupNames;
    EnableGroupButtons;
    EnableMapButtons;
  end
  else
    ErrorMessage('Unable to delete group:', GroupName);
end;

procedure TMainForm.CreateMap;

var
  MapName, FileName: string;
  MapFile: TextFile;

begin
  MapName := InputBox('Create Map', 'Map name:', '');
  if MapName = '' then
    Exit;

  if ContainsInvalidCharacters(MapName, InvalidNameChars) then
  begin
    ErrorMessage('Map names may not contain any of:', AddSpacesToString(InvalidNameChars));
    Exit;
  end;

  FileName := ConcatPaths([DataPath, ComboGroups.Text, MapName + MapFileExtension]);
  if FileExists(FileName) then
  begin
    ErrorMessage('Map already exists:', MapName);
    Exit;
  end;

  try
    AssignFile(MapFile, FileName);
    Rewrite(MapFile);
    CloseFile(MapFile);
    ReadMapNames;
    ComboMaps.Text := MapName; { This does not fire ComboMapsChange() so does not try to load
                                 the invalid (empty) map file }
    EnableMapButtons;
    ClearMap;
    ResetMapPosition;
    SaveMap; { Make the file valid }
  except
    on E: EInOutError do
      ErrorMessage('Unable to create map:', MapName);
  end;
end;

procedure TMainForm.RenameMap;

var
  MapName, NewMapName, GroupName, FileName, NewFileName: string;

begin
  MapName := ComboMaps.Text;
  NewMapName := InputBox('Rename Map', 'New map name:', MapName);
  if (NewMapName = MapName) or (NewMapName = '') then
    Exit;

  if ContainsInvalidCharacters(NewMapName, InvalidNameChars) then
  begin
    ErrorMessage('Map names may not contain any of:', AddSpacesToString(InvalidNameChars));
    Exit;
  end;

  GroupName := ComboGroups.Text;
  FileName := ConcatPaths([DataPath, GroupName, MapName + MapFileExtension]);
  NewFileName := ConcatPaths([DataPath, GroupName, NewMapName + MapFileExtension]);
  if FileExists(NewFileName) then
  begin
    ErrorMessage('Map already exists:', NewMapName);
    Exit;
  end;

  if RenameFile(FileName, NewFileName) then
  begin
    ReadMapNames;
    ComboMaps.Text := NewMapName;
  end
  else
    ErrorMessage('Unable to rename map:', MapName);
end;

procedure TMainForm.DeleteMap;

var
  MapName, Message: string;
  FileName: string;

begin
  MapName := ComboMaps.Text;
  Message := Format('Delete the map "%s"?', [MapName]);
  if Application.MessageBox(PChar(Message), 'Delete Map', MB_ICONQUESTION + MB_YESNO) <>
    idYes then
    Exit;

  FileName := ConcatPaths([DataPath, ComboGroups.Text, MapName + MapFileExtension]);
  if DeleteFile(FileName) then
  begin
    ReadMapNames;
    EnableMapButtons;
    ClearMap;
    Invalidate;
  end
  else
    ErrorMessage('Unable to delete map:', MapName);
end;

procedure TMainForm.Shutdown;

var
  I: integer;

begin
  SaveMap; { To save the grid size and position }
  SaveSettings;

  for I := 0 to UndoLevels do
  begin
    ClearTiles(MapTiles[I]);
    MapTiles[I].Free;
  end;

  ClearTiles(CopiedTiles);
  CopiedTiles.Free;
end;

procedure TMainForm.SaveSettings;

var
  FileName: string;
  SettingsFile: TextFile;

begin
  FileName := DataPath + 'dungeonmapper.conf';
  AssignFile(SettingsFile, FileName);
  try
    ReWrite(SettingsFile);
    WriteLn(SettingsFile, ComboGroups.Text);
    WriteLn(SettingsFile, ComboMaps.Text);
    WriteLn(SettingsFile, WindowTop);
    WriteLn(SettingsFile, WindowLeft);
    WriteLn(SettingsFile, WindowWidth);
    WriteLn(SettingsFile, WindowHeight);
    if WindowState = wsMaximized then
      WriteLn(SettingsFile, 'Y')
    else
      WriteLn(SettingsFile, 'N');
    CloseFile(SettingsFile);
  except
    on E: EInOutError do
      ErrorMessage(Format('Error writing settings file (%s):', [E.Message]), FileName);
  end;
end;

end.

