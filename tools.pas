unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figures, Graphics, GraphMath, LCLType,
  LCLIntf, LCL, Scale, ExtCtrls, StdCtrls, Spin, Dialogs, Buttons;

type

TParam = class
 procedure CreateObjects(Panel: TPanel; pos: Integer); virtual; abstract;
end;

 TPenColorParam = class(Tparam)
 procedure ChangePenColor(Sender: TObject);
 procedure CreateObjects(Panel: TPanel; pos: Integer); override;
end;

 TBrushColorParam = class(TParam)
  procedure ChangeBrushColor(Sender: TObject);
  procedure CreateObjects(Panel: TPanel; pos: Integer); override;
end;

 TWidthParam = class(TParam)
  procedure ChangeWidth(Sender: TObject);
  procedure CreateObjects(Panel: TPanel; pos: Integer); override;
end;

TRoundingRadiusParamX = class(TParam)
  procedure ChangeRoundX(Sender: TObject);
  procedure CreateObjects(Panel: TPanel; pos: Integer); override;
end;

TRoundingRadiusParamY = class(TParam)
  procedure ChangeRoundY(Sender: TObject);
  procedure CreateObjects(Panel: TPanel; pos: Integer); override;
end;

TBrushStyleParam = class(TParam)
  const BStyles: array [0..7] of TBrushStyle = (bsSolid, bsClear,
bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
  procedure ChangeBrushStyle(Sender: TObject);
  procedure CreateObjects(Panel: TPanel; pos: Integer); override;
end;

TPenStyleParam = class(TParam)
  procedure ChangePenStyle(Sender: TObject);
  procedure CreateObjects(Panel: TPanel; pos: Integer); override;
  const PStyles: array[0..5] of TPenStyle = (psSolid, psClear, psDot,
psDash, psDashDot, psDashDotDot);
end;

TFigureTool = class
  Points: array of TFloatPoint;
  Icons: string;
  Param: array of TParam;
  procedure MouseDown(AX: integer;AY: integer); virtual; abstract;
  procedure MouseMove(X: integer;Y: integer); virtual; abstract;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); virtual; abstract;
  procedure ParamListCreate(); virtual;abstract;
  procedure ParamsCreate(Panel: TPanel);
end;

TLineFigureTool = class(TFigureTool)
  procedure ParamListCreate(); override;
  procedure Mouseup(X: integer;Y: integer; ACanvas: TCanvas); override;
end;

TObjectFigureTool = class(TLineFigureTool)
  procedure ParamListCreate(); override;
end;

TPolyLineTool = class(TLineFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
end;

TLineTool = class(TLineFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
end;

TEllipceTool = class(TObjectFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
end;

TRectangleTool = class(TObjectFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
end;

TPawTool = class(TFigureTool)
  FirstPoint: TPoint;
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: Integer; Y:Integer; ACanvas: TCanvas); override;
  procedure ParamListCreate(); override;
end;

TMagnifierTool = class(TFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;
  procedure ParamListCreate(); override;
end;

TRoundedRectangleTool = class(TObjectFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure ParamListCreate(); override;
end;

TSelectTool = class(TFigureTool)
  SelectedChangeSize: Boolean;
  ChangeSizeIndex: Integer;
  APoint: TPoint;
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;
  procedure ParamListCreate(); override;
  procedure SelectParamListCreate();
end;
var
  Tool: array of TFigureTool;
  APenColor, ABrushColor: TColor;
  AWidth,ARadiusX,ARadiusY: integer;
  APenStyle: TPenStyle;
  ABrushStyle: TBrushStyle;
  SelectedBStyleIndex, SelectedPStyleIndex: integer;
  SelectedCreateParamFlag: Boolean;
  SelectedChangeSize: Boolean;
  SelectedFigureForChangingSize, SelectedPoint: integer;
  SelectedFigure: TFigureTool;
  Invalidate_: procedure of Object;

implementation
procedure TPenColorParam.CreateObjects(Panel: TPanel; pos: integer);
var
  ColorLabel: TLabel;
  PenColor: TColorButton;
begin
  ColorLabel := TLabel.Create(Panel);
  ColorLabel.Caption := 'Цвет карандаша';
  ColorLabel.Top := pos;
  ColorLabel.Parent:=Panel;

  PenColor := TColorButton.Create(panel);
  PenColor.Top := pos + 20;
  PenColor.Parent := Panel;
  PenColor.ButtonColor := APenColor;
  PenColor.OnColorChanged := @ChangePenColor;
end;

procedure TPenColorParam.ChangePenColor(Sender: TObject);
var
  i: Integer;
begin
  APenColor := (Sender as TColorButton).ButtonColor;
  for I:=0 to High(CurrentFigures) do
    if CurrentFigures[i].Selected then (CurrentFigures[i] as TLineFigure).PenColor := APenColor;
    Invalidate_;
    CreateArrayOfActions();
end;

procedure TPenStyleParam.CreateObjects(Panel: TPanel; pos: Integer);
var
  StyleLabel: TLabel;
  PenStyle: TComboBox;
  i: integer;
  s: string;
begin
  StyleLabel := TLabel.Create(Panel);
  StyleLabel.Caption := 'Стиль линии';
  StyleLabel.Top := pos;
  StyleLabel.Parent:=Panel;

  PenStyle  := TComboBox.Create(panel);
  for i:=0 to 5 do
  begin
    WriteStr(s, PStyles[i]);
    PenStyle.Items.Add(s);
  end;
  PenStyle.Top := pos + 20;
  PenStyle.Parent := Panel;
  PenStyle.ReadOnly := True;
  PenStyle.ItemIndex := SelectedPStyleIndex;
  PenStyle.OnChange := @ChangePenStyle;
end;

procedure TPenStyleParam.ChangePenStyle(Sender: TObject);
var
  i: integer;
begin
  APenStyle := PStyles[(Sender as TComboBox).ItemIndex];
  SelectedPStyleIndex := (Sender as TComboBox).ItemIndex;
  for I:=0 to High(CurrentFigures) do
  if CurrentFigures[i].Selected then (CurrentFigures[i] as TLineFigure).PenStyle := APenStyle;
   Invalidate_;
   CreateArrayOfActions();
end;

procedure TWidthParam.CreateObjects(Panel: TPanel; pos: Integer);
var
  WidthLabel: TLabel;
  WidthParam: TSpinEdit;
begin
  WidthLabel := TLabel.Create(Panel);
  WidthLabel.Caption := 'Ширина карандаша';
  WidthLabel.Top := pos;
  WidthLabel.Parent := Panel;

  WidthParam := TSpinEdit.Create(Panel);
  WidthParam.Top := pos + 20;
  WidthParam.MinValue := 1;
  WidthParam.Parent:= Panel;
  WidthParam.Value := AWidth;
  WidthParam.OnChange := @ChangeWidth;
end;

procedure TWidthParam.ChangeWidth(Sender: TObject);
var
  i: integer;
begin
 AWidth := (Sender as TSpinEdit).Value;
 for I:=0 to High(CurrentFigures) do
  if CurrentFigures[i].Selected then (CurrentFigures[i] as TLineFigure).Width := AWidth;
  Invalidate_;
  CreateArrayOfActions();
end;

procedure TBrushColorParam.CreateObjects(Panel: TPanel; pos: Integer);
var
  ColorLabel: TLabel;
  BrushColor: TColorButton;
begin
  ColorLabel := TLabel.Create(Panel);
  ColorLabel.Caption := 'Цвет заливки';
  ColorLabel.Top := pos;
  ColorLabel.Parent := Panel;

  BrushColor := TColorButton.Create(Panel);
  BrushColor.Top := pos + 20;
  BrushColor.Parent := Panel;
  BrushColor.ButtonColor := ABrushColor;
  BrushColor.OnColorChanged := @ChangeBrushColor;
end;

procedure TBrushColorParam.ChangeBrushColor(Sender: TObject);
var
  i: Integer;
begin
  ABrushColor := (Sender as TColorButton).ButtonColor;
  for I:=0 to High(CurrentFigures) do
    if (CurrentFigures[i].Selected) and not (CurrentFigures[i].Index = 1)
      then (CurrentFigures[i] as TObjectFigure).BrushColor := ABrushColor;
   Invalidate_;
   CreateArrayOfActions();
end;

procedure TBrushStyleParam.CreateObjects(Panel: TPanel; pos: Integer);
var
  StyleLabel: TLabel;
  BrushStyle: TComboBox;
  i: Integer;
  s: String;
begin
  StyleLabel := TLabel.Create(Panel);
  StyleLabel.Caption := 'Стиль заливки ';
  StyleLabel.Top := pos;
  StyleLabel.Parent:=Panel;

  BrushStyle := TComboBox.Create(panel);
  for i:=0 to 5 do
  begin
    WriteStr(s, BStyles[i]);
    BrushStyle.Items.Add(s);
  end;
  BrushStyle.Top := pos + 20;
  BrushStyle.Parent := Panel;
  BrushStyle.ItemIndex := SelectedBStyleIndex;
  BrushStyle.ReadOnly := True;
  BrushStyle.OnChange := @ChangeBrushStyle;
end;

procedure TBrushStyleParam.ChangeBrushStyle(Sender: TObject);
var
  i: integer;
begin
  ABrushStyle := BStyles[(Sender as TComboBox).ItemIndex];
  SelectedBStyleIndex := (Sender as TComboBox).ItemIndex;
  for I:=0 to High(CurrentFigures) do
    if (CurrentFigures[i].Selected) and not (CurrentFigures[i].Index = 1)
      then (CurrentFigures[i] as TObjectFigure).BrushStyle := ABrushStyle;
   Invalidate_;
   CreateArrayOfActions();
end;

procedure TRoundingRadiusParamX.CreateObjects(Panel: TPanel; pos: Integer);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusX: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(Panel);
  RoundingRadiusLabel.Caption := 'Радиус округления X';
  RoundingRadiusLabel.Top := pos;
  RoundingRadiusLabel.Parent := Panel;

  RoundingRadiusX := TSpinEdit.Create(Panel);
  RoundingRadiusX.Top := pos + 20;
  RoundingRadiusX.MinValue := 0;
  RoundingRadiusX.OnChange := @ChangeRoundX;
  RoundingRadiusX.Parent := Panel;
  RoundingRadiusX.Value := ARadiusX;
end;

procedure TRoundingRadiusParamX.ChangeRoundX(Sender: TObject);
var
  i: Integer;
begin
  ARadiusX := (Sender as TSpinEdit).Value;
  for I:=0 to High(CurrentFigures) do
    if (CurrentFigures[i].Selected) and (CurrentFigures[i].Index = 3)
      then (CurrentFigures[i] as TRoundedRectangle).RoundingRadiusX := ARadiusX;
   Invalidate_;
   CreateArrayOfActions();
end;

procedure TRoundingRadiusParamY.CreateObjects(Panel: TPanel; pos: Integer);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusY: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(Panel);
  RoundingRadiusLabel.Caption := 'Радиус округления Y';
  RoundingRadiusLabel.Top := pos;
  RoundingRadiusLabel.Parent :=Panel;

  RoundingRadiusY := TSpinEdit.Create(Panel);
  RoundingRadiusY.Top := pos + 20;
  RoundingRadiusY.MinValue := 0;
  RoundingRadiusY.Parent := Panel;
  RoundingRadiusY.Value := ARadiusY;
  RoundingRadiusY.OnChange := @ChangeRoundY;
end;

procedure TRoundingRadiusParamY.ChangeRoundY(Sender: TObject);
var
  i: Integer;
begin
  ARadiusY := (Sender as TSpinEdit).Value;
  for I:=0 to High(CurrentFigures) do
    if (CurrentFigures[i].Selected) and (CurrentFigures[i].Index = 3)
      then (CurrentFigures[i] as TRoundedRectangle).RoundingRadiusY := ARadiusY;
   Invalidate_;
   CreateArrayOfActions();
end;

procedure TLineFigureTool.ParamListCreate();
begin
  SetLength(Param, 0);
  SetLength(Param, Length(Param) + 3);
  Param[High(Param) - 2] := TPenColorParam.Create();
  Param[High(Param) - 1] := TPenStyleParam.Create();
  Param[High(Param)] := TWidthParam.Create();
end;

procedure TObjectFigureTool.ParamListCreate();
begin
  Inherited;
  SetLength(Param,Length(Param) + 2);
  Param[High(Param) - 1]:= TBrushColorParam.Create();
  Param[High(Param)]:= TBrushStyleParam.Create();
end;

procedure TRoundedRectangleTool.ParamListCreate();
begin
  Inherited;
  SetLength(Param,Length(Param) + 2);
  Param[High(Param)-1] := TRoundingRadiusParamX.Create();
  Param[High(Param)] := TRoundingRadiusParamY.Create();
end;

procedure TPawTool.ParamListCreate();
begin
end;

procedure TMagnifierTool.ParamListCreate();
begin
end;

procedure TSelectTool.ParamListCreate();
begin
end;
procedure TFigureTool.ParamsCreate(Panel: TPanel);
var
  i, pos: Integer;
begin
  ParamListCreate();
  For i:=0 to high(Param) do begin
    Param[i].CreateObjects(Panel, i * 60);
  end;
end;
procedure RegisterTool(ATool: TFigureTool; S: string);
begin
  Setlength(Tool, Length(Tool)+1);
  Tool[high(Tool)] := ATool;
  Tool[high(Tool)].Icons := s;
  Atool.ParamListCreate();
end;
procedure TLineFigureTool.MouseUp(X: Integer;Y: Integer; ACanvas: TCanvas);
begin
   CreateArrayOfActions();
end;
procedure TMagnifierTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
  RectZoom(AHeightPB,AWidthPB,(CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[0],(CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1]);
  SetLength(CurrentFigures, Length(CurrentFigures) - 1);
end;

procedure TMagnifierTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TRectangleMagnifier;
begin
  SetLength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangleMagnifier.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
end;

procedure TMagnifierTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
end;

procedure TPawTool.MouseDown(X: integer;Y: integer);
begin
  FirstPoint := Point(X,Y);
end;

procedure TPawTool.MouseMove(X: integer;Y: integer);
begin
  offset.x += FirstPoint.X - X;
  offset.y += FirstPoint.Y - Y;
  FirstPoint:=Point(X,Y);
end;

procedure TPawTool.MouseUp(X: Integer; Y:Integer; ACanvas: TCanvas);
begin

end;

procedure TPolyLineTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TLineFigure;
begin
  StopUndo;
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TPolyLine.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
  SetLength((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points, Length((CurrentFigures[high(CurrentFigures)] as TLineFigure).points) + 1);
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[high((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points)] := ScreenToWorld(Point(X,Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  AFigure.Index := 1;
  MaxMin(Point(X,Y));
end;

procedure TLineTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TLineFigure;
begin
  StopUndo;
  Setlength(CurrentFigures, length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TLine.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
  AFigure.Points[1] := ScreenToWorld(Point(X,Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  AFigure.Index := 1;
  MaxMin(Point(X,Y));
end;

procedure TRectangleTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TObjectFigure;
begin
  StopUndo;
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangle.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
  AFigure.Points[1] := ScreenToWorld(Point(X,Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  AFigure.Index := 2;
  MaxMin(Point(X,Y));
end;

procedure TRoundedRectangleTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TObjectFigure;
begin
  StopUndo;
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRoundedRectangle.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
  AFigure.Points[1] := ScreenToWorld(Point(X,Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.RoundingRadiusX := ARadiusX;
  AFigure.RoundingRadiusY:= ARadiusY;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  AFigure.Index := 3;
  MaxMin(Point(X,Y));
end;

procedure TEllipceTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TObjectFigure;
begin
  StopUndo;
  SetLength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TEllipce.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
  AFigure.Points[1] := ScreenToWorld(Point(X,Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  AFigure.Index := 2;
  MaxMin(Point(X,Y));
end;

procedure TLineTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
  MaxMin(Point(X,Y));
end;

procedure TEllipceTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
  MaxMin(Point(X,Y));
end;

procedure TRectangleTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
  MaxMin(Point(X,Y));
end;

procedure TRoundedRectangleTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
  MaxMin(Point(X,Y));
end;

procedure TPolyLineTool.MouseMove(X: integer;Y: integer);
begin
  SetLength((CurrentFigures[high(CurrentFigures)] as TLineFigure).points, length((CurrentFigures[high(CurrentFigures)] as TLineFigure).points) + 1);
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[high((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points)] := ScreenToWorld(Point(X,Y));
  MaxMin(Point(X,Y));
end;

procedure TSelectTool.MouseDown(X: Integer; Y: Integer);
var
  AFigure: TRectangleMagnifier;
  width, i, j: Integer;
begin
  for i:=0 to high(CurrentFigures) do begin
    width := (CurrentFigures[i] as TLineFigure).Width;

      if (x >= WorldToScreen(CurrentFigures[i].Points[0]).x - 15 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[0]).y - 15 - Width div 2) and
         (x <= WorldToScreen(CurrentFigures[i].Points[0]).x - 5 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[0]).y - 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 1;
       end;

      if (x <= WorldToScreen(CurrentFigures[i].Points[1]).x + 15 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[1]).y + 15 - Width div 2) and
         (x >= WorldToScreen(CurrentFigures[i].Points[1]).x + 5 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[1]).y + 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 2;
       end;

      if (x <= WorldToScreen(CurrentFigures[i].Points[0]).x + 15 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[1]).y - 15 - Width div 2) and
         (x >= WorldToScreen(CurrentFigures[i].Points[0]).x + 5 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[1]).y - 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 3;
       end;

      if (x >= WorldToScreen(CurrentFigures[i].Points[1]).x - 15 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[0]).y + 15 - Width div 2) and
         (x <= WorldToScreen(CurrentFigures[i].Points[1]).x - 5 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[0]).y + 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 4;
       end;

       if (x >= WorldToScreen(CurrentFigures[i].Points[0]).x + 15 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[0]).y + 15 - Width div 2) and
         (x <= WorldToScreen(CurrentFigures[i].Points[0]).x + 5 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[0]).y + 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 1;
       end;

      if (x <= WorldToScreen(CurrentFigures[i].Points[1]).x - 15 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[1]).y - 15 - Width div 2) and
         (x >= WorldToScreen(CurrentFigures[i].Points[1]).x - 5 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[1]).y - 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 2;
       end;

      if (x <= WorldToScreen(CurrentFigures[i].Points[2]).x - 15 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[1]).y + 15 - Width div 2) and
         (x >= WorldToScreen(CurrentFigures[i].Points[2]).x - 5 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[1]).y + 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 3;
       end;

      if (x >= WorldToScreen(CurrentFigures[i].Points[1]).x + 15 - Width div 2) and
         (y <= WorldToScreen(CurrentFigures[i].Points[2]).y - 15 - Width div 2) and
         (x <= WorldToScreen(CurrentFigures[i].Points[1]).x + 5 - Width div 2) and
         (y >= WorldToScreen(CurrentFigures[i].Points[2]).y - 5 - width div 2) and
         (length(CurrentFigures[i].Points) = 2) then begin
           SelectedFigureForChangingSize := i;
           SelectedChangeSize := True;
           ChangeSizeIndex := 4;
       end;


      if (length(CurrentFigures[i].Points) > 2) then begin
        for j:=0 to high(CurrentFigures[i].Points) do begin
          if (x >= WorldToScreen((CurrentFigures[i] as TPolyLine).Points[j]).x - 5 - Width div 2) and
             (y >= WorldToScreen((CurrentFigures[i] as TPolyLine).Points[j]).y - 5 - Width div 2) and
             (x <= WorldToScreen((CurrentFigures[i] as TPolyLine).Points[j]).x + 5 - Width div 2) and
             (y <= WorldToScreen((CurrentFigures[i] as TPolyLine).Points[j]).y + 5 - width div 2)
         then begin
               SelectedFigureForChangingSize := i;
               SelectedChangeSize := True;
               SelectedPoint := J;
               ChangeSizeIndex := 5;
          End;
       end;
    end;
  end;

  if not SelectedChangeSize then begin
  SetLength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangleMagnifier.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
  AFigure.Points[1] := ScreenToWorld(Point(X,Y));
  SelectedCreateParamFlag := True;
  end;
end;

procedure TSelectTool.MouseMove(X: Integer; Y: Integer);
var
  i, j: Integer;
begin
  if SelectedChangeSize then
begin
    case ChangeSizeIndex of
  1: begin
       CurrentFigures[SelectedFigureForChangingSize].Points[0].X := ScreenToWorld(Point(X,Y)).X;
       CurrentFigures[SelectedFigureForChangingSize].Points[0].Y := ScreenToWorld(Point(X,Y)).Y;
     end;
  2: begin
       CurrentFigures[SelectedFigureForChangingSize].Points[1].X := ScreenToWorld(Point(X,Y)).X;
       CurrentFigures[SelectedFigureForChangingSize].Points[1].Y := ScreenToWorld(Point(X,Y)).Y;
     end;
  3: begin
       CurrentFigures[SelectedFigureForChangingSize].Points[1].X := ScreenToWorld(Point(X,Y)).X;
       CurrentFigures[SelectedFigureForChangingSize].Points[2].Y := ScreenToWorld(Point(X,Y)).Y;
     end;
  4: begin
       CurrentFigures[SelectedFigureForChangingSize].Points[2].X := ScreenToWorld(Point(X,Y)).X;
       CurrentFigures[SelectedFigureForChangingSize].Points[1].Y := ScreenToWorld(Point(X,Y)).Y;
     end;
  5: begin
       CurrentFigures[SelectedFigureForChangingSize].Points[SelectedPoint].X := ScreenToWorld(Point(X,Y)).X;
       CurrentFigures[SelectedFigureForChangingSize].Points[SelectedPoint].Y := ScreenToWorld(Point(X,Y)).Y;
     end;
end;
    end;
 if not SelectedChangeSize then (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
end;

procedure TSelectTool.MouseUp(X: Integer; Y: Integer; ACanvas: TCanvas);
var
  SelectRegion: HRGN;
   i:integer;
  ToolRegio: HRGN;
begin
  if not SelectedChangeSize then begin
  SelectRegion := CreateRectRgn((WorldToScreen((CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier).Points[0]).x),
                                 (WorldToScreen((CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier).Points[0]).y),
                                 (WorldToScreen((CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier).Points[1]).x),
                                 (WorldToScreen((CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier).Points[1]).y));
     with CurrentFigures[high(CurrentFigures)] do begin
     for i := 0 to high(CurrentFigures)-1 do
    begin
        DeleteObject(CurrentFigures[i].Region);
        CurrentFigures[i].SetRegion;
        ToolRegio := CreateRectRgn(1,1,2,2);
        if (CombineRgn(ToolRegio,CurrentFigures[i].Region,SelectRegion,RGN_AND)
          <> NULLREGION) then
            begin
              if CurrentFigures[i].Selected = false then
                CurrentFigures[i].Selected := true
              else
                CurrentFigures[i].Selected := false;
            end;
        DeleteObject(ToolRegio);
    end;
     end;
  SetLength(CurrentFigures, Length(CurrentFigures) - 1);
  SelectParamListCreate();
  end;
  SelectedChangeSize := false;
  ChangeSizeIndex := 0;
  SelectedFigureForChangingSize := -1;
  SelectedPoint := -1;
end;

procedure TSelectTool.SelectParamListCreate();
var
  i: Integer;
  highIndex: Integer;
  f1: TLineFigureTool;
  f2: TObjectFigureTool;
  f3: TRoundedRectangleTool;
begin
  highIndex := 0;
   for i := 0 to high(CurrentFigures) do
     if CurrentFigures[i].Selected then
       if (CurrentFigures[i].Index > highIndex) then highIndex := CurrentFigures[i].Index;

   f1 := TLineFigureTool.Create();
   f2 := TObjectFigureTool.Create();
   f3 := TRoundedRectangleTool.Create();

   case highIndex of
     1: begin
          f1.ParamListCreate();
          SelectedFigure := f1;
        end;
     2: begin
          f2.ParamListCreate();
          SelectedFigure := f2;
        end;
     3: begin
          f3.ParamListCreate();
          SelectedFigure := f3;
        end;
   end;
end;
begin
  Setlength(Tool, 8);
  Tool[0] := TPolyLineTool.Create();
  Tool[0].Icons := 'TPolyLine';
  Tool[1] := TLineTool.Create();
  Tool[1].Icons := 'TLine';
  Tool[2] := TRectangleTool.Create();
  Tool[2].Icons := 'TRectangle';
  Tool[3] := TEllipceTool.Create();
  Tool[3].Icons := 'TEllipce';
  Tool[4] := TPawTool.Create();
  Tool[4].Icons := 'TPaw';
  Tool[5] := TMagnifierTool.Create();
  Tool[5].Icons := 'TMagnifier';
  Tool[6] := TRoundedRectangleTool.Create();
  Tool[6].Icons := 'TRoundedRectangle';
  Tool[7] := TSelectTool.Create();
  Tool[7].Icons := 'TSelect';
end.
