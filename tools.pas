unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figures, Graphics, GraphMath, toolsparams, LCLType,
  LCLIntf, LCL, Scale, ExtCtrls, StdCtrls, Spin, Dialogs, Buttons;

type
  TFigureTool = class
    Icons: string;
    Param: array of TParam;
    procedure MouseDown(AX: integer; AY: integer; AWidth: integer); virtual; abstract;
    procedure MouseMove(X: integer; Y: integer); virtual; abstract;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); virtual; abstract;
    procedure ParamListCreate(); virtual; abstract;
    procedure ParamsCreate(Panel: TPanel); virtual;
  end;

  TLineFigureTool = class(TFigureTool)
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
  end;

  TObjectFigureTool = class(TLineFigureTool)
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
  end;

  TPolyLineTool = class(TLineFigureTool)
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;

  end;

  TLineTool = class(TLineFigureTool)
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;

  end;

  TEllipseTool = class(TObjectFigureTool)
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;

  end;

  TRectangleTool = class(TObjectFigureTool)
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;

  end;

  TMagnifierTool = class(TFigureTool)
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure ParamListCreate(); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
  end;

  TRoundedRectangleTool = class(TObjectFigureTool)
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
  end;

  TSelectTool = class(TFigureTool)
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
    procedure SelectParamListCreate();
  end;

  TPawTool = class(TFigureTool)
    APoint: TPoint;
    procedure MouseDown(X: integer; Y: integer; AWidth: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure Mouseup(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
  end;

var
  Tool: array of TFigureTool;
  SelectedCreateParamFlag: boolean;
  SelectedFigure: TFigureTool;

implementation

procedure TLineFigureTool.ParamListCreate();
begin
  SetLength(Param, Length(Param) + 3);
  Param[High(Param) - 2] := TPenColorParam.Create();
  Param[High(Param) - 1] := TPenStyleParam.Create();
  Param[High(Param)] := TWidthParam.Create();
end;

procedure TObjectFigureTool.ParamListCreate();
begin
  inherited;
  SetLength(Param, Length(Param) + 2);
  Param[High(Param) - 1] := TBrushColorParam.Create();
  Param[High(Param)] := TBrushStyleParam.Create();
end;

procedure TRoundedRectangleTool.ParamListCreate();
begin
  inherited;
  SetLength(Param, Length(Param) + 2);
  Param[High(Param) - 1] := TRoundingRadiusParamX.Create();
  Param[High(Param)] := TRoundingRadiusParamY.Create();
end;

procedure TMagnifierTool.ParamListCreate();
begin
end;

procedure TFigureTool.ParamsCreate(Panel: TPanel);
var
  i: integer;
begin
  ParamListCreate();
  for i := 0 to high(Param) do
  begin
    Param[i].CreateObjects(Panel);
  end;
end;

procedure TLineFigureTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TObjectFigureTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TEllipseTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TPolyLineTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TRectangleTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure Tlinetool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TRoundedRectangleTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TMagnifierTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
  RectZoom(AHeightPB, AWidthPB, (CurrentFigures[high(CurrentFigures)] as
    TLineFigure).Points[0], (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1]);
  SetLength(CurrentFigures, Length(CurrentFigures) - 1);
end;

procedure TMagnifierTool.MouseDown(X: integer; Y: integer; AWidth: integer);
var
  AFigure: TRectangleMagnifier;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangleMagnifier.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
end;

procedure TMagnifierTool.MouseMove(X: integer; Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] :=
    ScreenToWorld(Point(X, Y));
end;

procedure TPolyLineTool.MouseDown(X: integer; Y: integer; AWidth: integer);
var
  AFigure: TLineFigure;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TPolyLine.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
  SetLength((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points,
    Length((CurrentFigures[high(CurrentFigures)] as TLineFigure).points) + 1);
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[high(
    (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points)] :=
    ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  MaxMin(Point(X, Y));
end;

procedure TLineTool.MouseDown(X: integer; Y: integer; AWidth: integer);
var
  AFigure: TLineFigure;
begin
  Setlength(CurrentFigures, length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TLine.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  MaxMin(Point(X, Y));
end;

procedure TRectangleTool.MouseDown(X: integer; Y: integer; AWidth: integer);
var
  AFigure: TObjectFigure;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangle.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  MaxMin(Point(X, Y));
end;

procedure TEllipseTool.MouseDown(X: integer; Y: integer; AWidth: integer);
var
  AFigure: TObjectFigure;
begin
  SetLength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TEllipce.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  MaxMin(Point(X, Y));
end;

procedure TRoundedRectangleTool.MouseDown(X: integer; Y: integer; AWidth: integer);
var
  AFigure: TObjectFigure;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRoundedRectangle.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TObjectFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.RoundingRadiusX := ARadiusX;
  AFigure.RoundingRadiusY := ARadiusY;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  MaxMin(Point(X, Y));
end;

procedure TLineTool.MouseMove(X: integer; Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] :=
    ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TEllipseTool.MouseMove(X: integer; Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] :=
    ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TRectangleTool.MouseMove(X: integer; Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] :=
    ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TPolyLineTool.MouseMove(X: integer; Y: integer);
begin
  SetLength((CurrentFigures[high(CurrentFigures)] as TLineFigure).points,
    length((CurrentFigures[high(CurrentFigures)] as TLineFigure).points) + 1);
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[high(
    (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points)] :=
    ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TRoundedRectangleTool.MouseMove(X: integer; Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] :=
    ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TSelectTool.MouseDown(X: integer; Y: integer; AWidth: integer);
var
  AFigure: TRectangleMagnifier;
begin
  SetLength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangleMagnifier.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  SelectedCreateParamFlag := True;
end;

procedure TSelectTool.MouseMove(X: integer; Y: integer);
begin
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[1] :=
    ScreenToWorld(Point(X, Y));
  /////////
end;

procedure TSelectTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
var
  SelectRegion: HRGN;
  i: integer;
  ToolRegio: HRGN;
begin
  SelectRegion := CreateRectRgn(
    (WorldToScreen((CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier).Points[0]).x), (WorldToScreen((CurrentFigures[high(CurrentFigures)] as
    TRectangleMagnifier).Points[0]).y),
    (WorldToScreen((CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier).Points[1]).x), (WorldToScreen((CurrentFigures[high(CurrentFigures)] as
    TRectangleMagnifier).Points[1]).y));
  with CurrentFigures[high(CurrentFigures)] do
  begin
    for i := 0 to high(CurrentFigures) - 1 do
    begin
      DeleteObject(CurrentFigures[i].Region);
      CurrentFigures[i].SetRegion;
      ToolRegio := CreateRectRgn(1, 1, 2, 2);
      if (CombineRgn(ToolRegio, CurrentFigures[i].Region, SelectRegion, RGN_AND) <>
        NULLREGION) then
      begin
        if CurrentFigures[i].Selected = False then
          CurrentFigures[i].Selected := True
        else
          CurrentFigures[i].Selected := False;
      end;
      DeleteObject(ToolRegio);
    end;
  end;
  SetLength(CurrentFigures, Length(CurrentFigures) - 1);
  SelectParamListCreate();
end;

procedure TSelectTool.SelectParamListCreate();
var
  i: integer;
  highIndex: integer;
  f1: TLineFigureTool;
  f2: TObjectFigureTool;
  f3: TRoundedRectangleTool;
begin
  highIndex := 0;
  for i := 0 to high(CurrentFigures) do
    if CurrentFigures[i].Selected then
      if (CurrentFigures[i].Index > highIndex) then
        highIndex := CurrentFigures[i].Index;
  f1 := TLineFigureTool.Create();
  f2 := TObjectFigureTool.Create();
  f3 := TRoundedRectangleTool.Create();
  case highIndex of
    1:
    begin
      f1.ParamListCreate();
      SelectedFigure := f1;
    end;
    2:
    begin
      f2.ParamListCreate();
      SelectedFigure := f2;
    end;
    3:
    begin
      f3.ParamListCreate();
      SelectedFigure := f3;
    end;
  end;
end;

procedure TPawTool.MouseDown(X: integer;Y: integer;AWidth: integer);
begin
  APoint := Point(X,Y);
end;

procedure TPawTool.MouseMove(X: integer;Y: integer);
begin
  offset.x += APoint.X - X;
  offset.y += APoint.Y - Y;
  APoint:=Point(X,Y);
end;

procedure TPawTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TSelectTool.ParamListCreate();
begin
end;

procedure TPawTool.ParamListCreate();
begin
end;

begin
  Setlength(Tool, 8);
  Tool[0] := TPolyLineTool.Create();
  Tool[0].Icons := 'TPolyLine';
  Tool[1] := TLineTool.Create();
  Tool[1].Icons := 'TLine';
  Tool[2] := TRectangleTool.Create();
  Tool[2].Icons := 'TRectangle';
  Tool[3] := TEllipseTool.Create();
  Tool[3].Icons := 'TEllipse';
  Tool[4] := TPawTool.Create();
  Tool[4].Icons := 'TPaw';
  Tool[5] := TMagnifierTool.Create();
  Tool[5].Icons := 'TMagnifier';
  Tool[6] := TRoundedRectangleTool.Create();
  Tool[6].Icons := 'TRoundedRectangle';
  Tool[7] := TSelectTool.Create();
  Tool[7].Icons := 'TSelect';
end.
