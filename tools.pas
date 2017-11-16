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
 procedure CreateObjects(ParamsPanel: TPanel); override;
end;

 TBrushColorParam = class(TParam)
  procedure ChangeBrushColor(Sender: TObject);
  procedure CreateObjects(ParamsPanel: TPanel); override;
end;

 TWidthParam = class(TParam)
  procedure ChangeWidth(Sender: TObject);
  procedure CreateObjects(ParamsPanel: TPanel); override;
end;

 TRoundingRadiusParamX = class(TParam)
  procedure ChangeRoundX(Sender: TObject);
  procedure CreateObjects(ParamsPanel: TPanel); override;
end;

TRoundingRadiusParamY = class(TParam)
  procedure ChangeRoundY(Sender: TObject);
  procedure CreateObjects(ParamsPanel: TPanel); override;
  end;
TFigureTool = class
  Icons: string;
  Param: array of TParam;
  procedure MouseDown(AX: integer;AY: integer); virtual; abstract;
  procedure MouseMove(X: integer;Y: integer); virtual; abstract;
  procedure MouseUp(X: integer;Y: integer); virtual;abstract;
  procedure ParamListCreate();virtual;abstract;
  procedure ParamsCreate(ParamsPanel: TPanel);virtual;
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
  AWidth,ARadiusX,ARadiusY: integer;
implementation
procedure TPenColorParam.CreateObjects(ParamsPanel: TPanel);
var
  ColorLabel: TLabel;
  PenColor: TColorButton;
begin
  ColorLabel := TLabel.Create(ParamsPanel);
  ColorLabel.Caption := 'Цвет карандаша';
  ColorLabel.Top := 0;
  ColorLabel.Parent:=ParamsPanel;

  PenColor := TColorButton.Create(ParamsPanel);
  PenColor.Top := 20;
  PenColor.Parent := ParamsPanel;
  PenColor.OnColorChanged := @ChangePenColor;
end;

procedure TPenColorParam.ChangePenColor(Sender: TObject);
begin
  APenColor := (Sender as TColorButton).ButtonColor;
end;

procedure TBrushColorParam.CreateObjects(ParamsPanel: TPanel);
var
  ColorLabel: TLabel;
  BrushColor: TColorButton;
begin
  ColorLabel := TLabel.Create(ParamsPanel);
  ColorLabel.Caption := 'Цвет заливки';
  ColorLabel.Top := 120;
  ColorLabel.Parent := ParamsPanel;

  BrushColor := TColorButton.Create(ParamsPanel);
  BrushColor.Top := 140;
  BrushColor.Parent := ParamsPanel;
  BrushColor.OnColorChanged := @ChangeBrushColor;
end;

procedure TBrushColorParam.ChangeBrushColor(Sender: TObject);
begin
  ABrushColor := (Sender as TColorButton).ButtonColor;
end;

procedure TWidthParam.CreateObjects(ParamsPanel: TPanel);
var
  WidthLabel: TLabel;
  WidthParam: TSpinEdit;
begin
  WidthLabel := TLabel.Create(ParamsPanel);
  WidthLabel.Caption := 'Ширина карандаша';
  WidthLabel.Top := 60;
  WidthLabel.Parent := ParamsPanel;

  WidthParam := TSpinEdit.Create(ParamsPanel);
  WidthParam.Top := 80;
  WidthParam.MinValue := 1;
  WidthParam.Parent:= ParamsPanel;
  WidthParam.Value := AWidth;
  WidthParam.OnChange := @ChangeWidth;
end;

procedure TWidthParam.ChangeWidth(Sender: TObject);
begin
 AWidth := (Sender as TSpinEdit).Value;
end;

procedure TRoundingRadiusParamX.CreateObjects(ParamsPanel: TPanel);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusX: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(ParamsPanel);
  RoundingRadiusLabel.Caption := 'Радиус округления X';
  RoundingRadiusLabel.Top := 180;
  RoundingRadiusLabel.Parent := ParamsPanel;

  RoundingRadiusX := TSpinEdit.Create(ParamsPanel);
  RoundingRadiusX.Top := 200;
  RoundingRadiusX.MinValue := 0;
  RoundingRadiusX.Parent := ParamsPanel;
  RoundingRadiusX.Value := ARadiusX;
  RoundingRadiusX.OnChange := @ChangeRoundX;
end;

procedure TRoundingRadiusParamX.ChangeRoundX(Sender: TObject);
begin
  ARadiusX := (Sender as TSpinEdit).Value;
end;

procedure TRoundingRadiusParamY.CreateObjects(ParamsPanel: TPanel);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusY: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(ParamsPanel);
  RoundingRadiusLabel.Caption := 'Радиус округления Y';
  RoundingRadiusLabel.Top := 240;
  RoundingRadiusLabel.Parent :=ParamsPanel;

  RoundingRadiusY := TSpinEdit.Create(ParamsPanel);
  RoundingRadiusY.Top := 260;
  RoundingRadiusY.MinValue := 0;
  RoundingRadiusY.Parent := ParamsPanel;
  RoundingRadiusY.Value := ARadiusY;
  RoundingRadiusY.OnChange := @ChangeRoundY;
end;

procedure TRoundingRadiusParamY.ChangeRoundY(Sender: TObject);
begin
  ARadiusY := (Sender as TSpinEdit).Value;
end;
procedure TLineFigureTool.ParamListCreate();
begin
  SetLength(Param, Length(Param) + 2);
  Param[High(Param)-1] := TPenColorParam.Create();
  Param[High(Param)] := TWidthParam.Create();
end;

procedure TObjectFigureTool.ParamListCreate();
begin
  Inherited;
  SetLength(Param,Length(Param) + 1);
  Param[High(Param)]:= TBrushColorParam.Create();
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
procedure TFigureTool.ParamsCreate(ParamsPanel: TPanel);
var
  i: Integer;
begin
  For i:=0 to high(Param) do begin
    Param[i].CreateObjects(ParamsPanel);
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
