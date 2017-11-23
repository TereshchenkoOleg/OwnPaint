unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figures, Graphics, GraphMath, Scale, ExtCtrls, StdCtrls, Spin,Dialogs,Buttons;

type
TParam = class
 procedure CreateObjects(Panel: TPanel); virtual; abstract;
end;

 TPenColorParam = class(TParam)
 procedure ChangePenColor(Sender: TObject);
 procedure CreateObjects(Panel: TPanel); override;
end;

TBrushColorParam = class(TParam)
  procedure ChangeBrushColor(Sender: TObject);
  procedure CreateObjects(Panel: TPanel); override;
end;

 TWidthParam = class(TParam)
  procedure ChangeWidth(Sender: TObject);
  procedure CreateObjects(Panel: TPanel); override;
end;

 TRoundingRadiusParamX = class(TParam)
  procedure ChangeRoundX(Sender: TObject);
  procedure CreateObjects(Panel: TPanel); override;
end;

TRoundingRadiusParamY = class(TParam)
  procedure ChangeRoundY(Sender: TObject);
  procedure CreateObjects(Panel: TPanel); override;
  end;
TBrushStyleParam = class(TParam)
  const BStyles: array [0..7] of TBrushStyle = (bsSolid, bsClear,
bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
  procedure ChangeBrushStyle(Sender: TObject);
  procedure CreateObjects(Panel: TPanel); override;
end;

TPenStyleParam = class(TParam)
  procedure ChangePenStyle(Sender: TObject);
  procedure CreateObjects(Panel: TPanel); override;
  const PStyles: array[0..5] of TPenStyle = (psSolid, psClear, psDot,
psDash, psDashDot, psDashDotDot);
end;
TFigureTool = class
  Icons: string;
  Param: array of TParam;
  procedure MouseDown(AX: integer;AY: integer); virtual; abstract;
  procedure MouseMove(X: integer;Y: integer); virtual; abstract;
  procedure MouseUp(X: integer;Y: integer); virtual;abstract;
  procedure ParamListCreate();virtual;abstract;
  procedure ParamsCreate(Panel: TPanel);virtual;
end;

TLineFigureTool = class(TFigureTool)
  procedure MouseUp(X: integer; Y: integer); override;
  procedure ParamListCreate(); override;
end;

TObjectFigureTool = class(TLineFigureTool)
  procedure MouseUp(X: integer;Y: integer); override;
  procedure ParamListCreate(); override;
end;

TPolyLineTool = class(TLineFigureTool)
  procedure MouseDown(AX: integer;AY: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer); override;

end;

TLineTool = class(TLineFigureTool)
  procedure MouseDown(AX: integer;AY: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer); override;

end;

TEllipseTool = class(TObjectFigureTool)
  procedure MouseDown(AX: integer;AY: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer); override;

end;

TRectangleTool = class(TObjectFigureTool)
  procedure MouseDown(AX: integer;AY: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer); override;

end;

TPawTool = class(TFigureTool)
  FirstPoint: TPoint;
  procedure MouseDown(AX: integer;AY: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer); override;
  procedure ParamListCreate(); override;
end;

TMagnifierTool = class(TFigureTool)
  procedure MouseDown(AX: integer;AY: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure ParamListCreate(); override;
  procedure MouseUp(X: integer;Y: integer); override;
end;
TRoundedRectangleTool = class(TObjectFigureTool)
  procedure MouseDown(AX: integer;AY: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer); override;
  procedure ParamListCreate(); override;
end;

var
  Tool: array of TFigureTool;
  APenColor, ABrushColor: TColor;
  APenStyle: TPenStyle;
  ABrushStyle: TBrushStyle;
  AWidth,ARadiusX,ARadiusY: integer;
  SelectedBStyleIndex, SelectedPStyleIndex: integer;
implementation
procedure TPenColorParam.CreateObjects(Panel: TPanel);
var
  ColorLabel: TLabel;
  PenColor: TColorButton;
begin
  ColorLabel := TLabel.Create(Panel);
  ColorLabel.Caption := 'Цвет карандаша';
  ColorLabel.Top :=0;
  ColorLabel.Parent:=Panel;

  PenColor := TColorButton.Create(Panel);
  PenColor.Top := 20;
  PenColor.Parent := Panel;
  PenColor.ButtonColor := APenColor;
  PenColor.OnColorChanged := @ChangePenColor;
end;

procedure TPenColorParam.ChangePenColor(Sender: TObject);
begin
  APenColor := (Sender as TColorButton).ButtonColor;
end;
procedure TPenStyleParam.CreateObjects(Panel: TPanel);
var
  StyleLabel: TLabel;
  PenStyle: TComboBox;
  i: integer;
  s: string;
begin
  StyleLabel := TLabel.Create(Panel);
  StyleLabel.Caption := 'Стиль линии';
  StyleLabel.Top := 120;
  StyleLabel.Parent:=Panel;

  PenStyle  := TComboBox.Create(panel);
  for i:=0 to 5 do
  begin
    WriteStr(s, PStyles[i]);
    PenStyle.Items.Add(s);
  end;
  PenStyle.Top := 140;
  PenStyle.Parent := Panel;
  PenStyle.ReadOnly := True;
  PenStyle.ItemIndex := SelectedPStyleIndex;
  PenStyle.OnChange := @ChangePenStyle;
end;

procedure TPenStyleParam.ChangePenStyle(Sender: TObject);
begin
  APenStyle := PStyles[(Sender as TComboBox).ItemIndex];
  SelectedPStyleIndex := (Sender as TComboBox).ItemIndex;
end;
procedure TBrushColorParam.CreateObjects(Panel: TPanel);
var
  ColorLabel: TLabel;
  BrushColor: TColorButton;
begin
  ColorLabel := TLabel.Create(Panel);
  ColorLabel.Caption := 'Цвет заливки';
  ColorLabel.Top := 80;
  ColorLabel.Parent := Panel;

  BrushColor := TColorButton.Create(Panel);
  BrushColor.Top := 100;
  BrushColor.Parent := Panel;
  BrushColor.ButtonColor := ABrushColor;
  BrushColor.OnColorChanged := @ChangeBrushColor;
end;

procedure TBrushColorParam.ChangeBrushColor(Sender: TObject);
begin
  ABrushColor := (Sender as TColorButton).ButtonColor;
end;

procedure TWidthParam.CreateObjects(Panel: TPanel);
var
  WidthLabel: TLabel;
  WidthParam: TSpinEdit;
begin
  WidthLabel := TLabel.Create(Panel);
  WidthLabel.Caption := 'Ширина карандаша';
  WidthLabel.Top := 40;
  WidthLabel.Parent := Panel;

  WidthParam := TSpinEdit.Create(Panel);
  WidthParam.Top := 60;
  WidthParam.MinValue := 1;
  WidthParam.Parent:= Panel;
  WidthParam.Value := AWidth;
  WidthParam.OnChange := @ChangeWidth;
end;

procedure TBrushStyleParam.CreateObjects(Panel: TPanel);
var
  StyleLabel: TLabel;
  BrushStyle: TComboBox;
  i: Integer;
  s: String;
begin
  StyleLabel := TLabel.Create(Panel);
  StyleLabel.Caption := 'Стиль линии';
  StyleLabel.Top := 160;
  StyleLabel.Parent:=Panel;

  BrushStyle := TComboBox.Create(panel);
  for i:=0 to 6 do
  begin
    WriteStr(s, BStyles[i]);
    BrushStyle.Items.Add(s);
  end;
  BrushStyle.Top := 180;
  BrushStyle.Parent := Panel;
  BrushStyle.ItemIndex := SelectedBStyleIndex;
  BrushStyle.ReadOnly := True;
  BrushStyle.OnChange := @ChangeBrushStyle;
end;

procedure TBrushStyleParam.ChangeBrushStyle(Sender: TObject);
begin
  ABrushStyle := BStyles[(Sender as TComboBox).ItemIndex];
  SelectedBStyleIndex := (Sender as TComboBox).ItemIndex;
end;
procedure TWidthParam.ChangeWidth(Sender: TObject);
begin
 AWidth := (Sender as TSpinEdit).Value;
end;

procedure TRoundingRadiusParamX.CreateObjects(Panel: TPanel);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusX: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(Panel);
  RoundingRadiusLabel.Caption := 'Радиус округления X';
  RoundingRadiusLabel.Top := 200;
  RoundingRadiusLabel.Parent := Panel;

  RoundingRadiusX := TSpinEdit.Create(Panel);
  RoundingRadiusX.Top := 220;
  RoundingRadiusX.MinValue := 0;
  RoundingRadiusX.Parent := Panel;
  RoundingRadiusX.Value := ARadiusX;
  RoundingRadiusX.OnChange := @ChangeRoundX;
end;

procedure TRoundingRadiusParamX.ChangeRoundX(Sender: TObject);
begin
  ARadiusX := (Sender as TSpinEdit).Value;
end;

procedure TRoundingRadiusParamY.CreateObjects(Panel: TPanel);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusY: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(Panel);
  RoundingRadiusLabel.Caption := 'Радиус округления Y';
  RoundingRadiusLabel.Top := 240;
  RoundingRadiusLabel.Parent :=Panel;

  RoundingRadiusY := TSpinEdit.Create(Panel);
  RoundingRadiusY.Top := 260;
  RoundingRadiusY.MinValue := 0;
  RoundingRadiusY.Parent := Panel;
  RoundingRadiusY.Value := ARadiusY;
  RoundingRadiusY.OnChange := @ChangeRoundY;
end;

procedure TRoundingRadiusParamY.ChangeRoundY(Sender: TObject);
begin
  ARadiusY := (Sender as TSpinEdit).Value;
end;
procedure TLineFigureTool.ParamListCreate();
begin
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
procedure TFigureTool.ParamsCreate(Panel: TPanel);
var
  i: Integer;
begin
  ParamListCreate();
  For i:=0 to high(Param) do begin
    Param[i].CreateObjects(Panel);
  end;
end;
procedure TLineFigureTool.MouseUp(X: integer;Y: integer);
begin
end;

procedure TObjectFigureTool.MouseUp(X: integer;Y: integer);
begin
end;

procedure TEllipseTool.MouseUp(X: integer;Y: integer);
begin
end;

procedure TPolyLineTool.MouseUp(X: integer;Y: integer);
begin
end;

procedure TRectangleTool.MouseUp(X: integer;Y: integer);
begin
end;

procedure Tlinetool.MouseUp(X: integer;Y: integer);
begin
end;
procedure TRoundedRectangleTool.MouseUp(X: integer;Y: integer);
begin
end;
procedure TPawTool.MouseUp(X: integer;Y: integer);
begin
end;
procedure TMagnifierTool.MouseUp(X: integer;Y: integer);
begin
 RectZoom(AHeightPB,AWidthPB,(CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[0],(CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1]);
 SetLength(CurrentFigures, Length(CurrentFigures) - 1);
end;

procedure TMagnifierTool.MouseDown(AX: integer;AY: integer);
var
  AFigure: TRectangleMagnifier;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangleMagnifier.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(AX,AY));
  AFigure.Points[1] := ScreenToWorld(Point(AX,AY));
end;

procedure TMagnifierTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
end;

procedure TPawTool.MouseDown(AX: integer;AY: integer);
begin
  FirstPoint := Point(AX,AY);
end;

procedure TPawTool.MouseMove(X: integer;Y: integer);
begin
  offset.x += FirstPoint.X - X;
  offset.y += FirstPoint.Y - Y;
  FirstPoint:=Point(X,Y);
end;

procedure TPolyLineTool.MouseDown(AX: integer;AY: integer);
var
  AFigure: TLineFigure;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TPolyLine.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
  SetLength((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points, Length((CurrentFigures[high(CurrentFigures)] as TLineFigure).points) + 1);
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[high((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points)] := ScreenToWorld(Point(AX,AY));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  MaxMin(Point(AX,AY));
end;

procedure TLineTool.MouseDown(AX: integer;AY: integer);
var
  AFigure: TLineFigure;
begin
 Setlength(CurrentFigures, length(CurrentFigures) + 1);
 CurrentFigures[high(CurrentFigures)] := TLine.Create();
 AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
 SetLength(AFigure.Points, 2);
 AFigure.Points[0] := ScreenToWorld(Point(AX,AY));
 AFigure.Points[1] := ScreenToWorld(Point(AX,AY));
 AFigure.PenColor := APenColor;
 AFigure.Width := AWidth;
 AFigure.PenStyle := APenStyle;
 MaxMin(Point(AX,AY));
end;

procedure TRectangleTool.MouseDown(AX: integer;AY: integer);
var
  AFigure: TObjectFigure;
begin
 Setlength(CurrentFigures, Length(CurrentFigures) + 1);
 CurrentFigures[high(CurrentFigures)] := TRectangle.Create();
 AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
 SetLength(AFigure.Points, 2);
 AFigure.Points[0] := ScreenToWorld(Point(AX,AY));
 AFigure.Points[1] := ScreenToWorld(Point(AX,AY));
 AFigure.PenColor := APenColor;
 AFigure.Width := AWidth;
 AFigure.BrushColor := ABrushColor;
 AFigure.PenStyle := APenStyle;
 AFigure.BrushStyle := ABrushStyle;
 MaxMin(Point(AX,AY));
end;

procedure TEllipseTool.MouseDown(AX: integer;AY: integer);
var
  AFigure: TObjectFigure;
begin
 SetLength(CurrentFigures, Length(CurrentFigures) + 1);
 CurrentFigures[high(CurrentFigures)] := TEllipce.Create();
 AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
 SetLength(AFigure.Points, 2);
 AFigure.Points[0] := ScreenToWorld(Point(AX,AY));
 AFigure.Points[1] := ScreenToWorld(Point(AX,AY));
 AFigure.PenColor := APenColor;
 AFigure.Width := AWidth;
 AFigure.BrushColor := ABrushColor;
 AFigure.PenStyle := APenStyle;
 AFigure.BrushStyle := ABrushStyle;
 MaxMin(Point(AX,AY));
end;
procedure TRoundedRectangleTool.MouseDown(AX: integer;AY: integer);
var
  AFigure: TObjectFigure;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRoundedRectangle.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(AX,AY));
  AFigure.Points[1] := ScreenToWorld(Point(AX,AY));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.RoundingRadiusX := ARadiusX;
  AFigure.RoundingRadiusY:= ARadiusY;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  MaxMin(Point(AX,AY));
end;
procedure TLineTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
  MaxMin(Point(X,Y));
end;

procedure TEllipseTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
    MaxMin(Point(X,Y));
end;

procedure TRectangleTool.MouseMove(X: integer;Y: integer);
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
procedure TRoundedRectangleTool.MouseMove(X: integer;Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] := ScreenToWorld(Point(X,Y));
  MaxMin(Point(X,Y));
end;
begin
  Setlength(Tool, 7);
  Tool[0]:= TPolyLineTool.Create();
  Tool[0].Icons:='TPolyLine';
  Tool[1]:= TLineTool.Create();
  Tool[1].Icons:='TLine';
  Tool[2]:= TRectangleTool.Create();
  Tool[2].Icons:='TRectangle';
  Tool[3]:= TEllipseTool.Create();
  Tool[3].Icons:='TEllipse';
  Tool[4]:= TPawTool.Create();
  Tool[4].Icons:='TPaw';
  Tool[5]:=TMagnifierTool.Create();
  Tool[5].Icons:='TMagnifier';
  Tool[6]:=TRoundedRectangleTool.Create();
  Tool[6].Icons:='TRoundedRectangle';
end.
