unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figures, Graphics, GraphMath, toolsparams, LCLType, LCLIntf, LCL, Scale, ExtCtrls, StdCtrls, Spin, Dialogs,Buttons;

type
TFigureTool = class
  Icons: string;
  Param: array of TParam;
  procedure MouseDown(X: integer;Y: integer); virtual; abstract;
  procedure MouseMove(X: integer;Y: integer); virtual; abstract;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); virtual;abstract;
  procedure ParamListCreate();virtual;abstract;
  procedure ParamsCreate(Panel: TPanel);virtual;
end;

TLineFigureTool = class(TFigureTool)
  procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
  procedure ParamListCreate(); override;
end;

TObjectFigureTool = class(TLineFigureTool)
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;
  procedure ParamListCreate(); override;
end;

TPolyLineTool = class(TLineFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;

end;

TLineTool = class(TLineFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;

end;

TEllipseTool = class(TObjectFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;

end;

TRectangleTool = class(TObjectFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;

end;

TPawTool = class(TFigureTool)
  FirstPoint: TPoint;
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;
  procedure ParamListCreate(); override;
end;

TMagnifierTool = class(TFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure ParamListCreate(); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;
end;
TRoundedRectangleTool = class(TObjectFigureTool)
  procedure MouseDown(X: integer;Y: integer); override;
  procedure MouseMove(X: integer;Y: integer); override;
  procedure MouseUp(X: integer;Y: integer; ACanvas: TCanvas); override;
  procedure ParamListCreate(); override;
end;

var
  Tool: array of TFigureTool;
    AWidth: integer;
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
procedure TLineFigureTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
end;

procedure TObjectFigureTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
end;

procedure TEllipseTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
end;

procedure TPolyLineTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
end;

procedure TRectangleTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
end;

procedure Tlinetool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
end;
procedure TRoundedRectangleTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
end;
procedure TPawTool.MouseUp(X: integer;Y: integer; ACanvas: TCanvas);
begin
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
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TRectangleMagnifier.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
  AFigure.Points[1] := ScreenToWorld(Point(X,Y));
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

procedure TPolyLineTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TLineFigure;
begin
  Setlength(CurrentFigures, Length(CurrentFigures) + 1);
  CurrentFigures[high(CurrentFigures)] := TPolyLine.Create();
  AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
  SetLength((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points, Length((CurrentFigures[high(CurrentFigures)] as TLineFigure).points) + 1);
  (CurrentFigures[high(CurrentFigures)] as TLineFigure).Points[high((CurrentFigures[high(CurrentFigures)] as TLineFigure).Points)] := ScreenToWorld(Point(X,Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  MaxMin(Point(X,Y));
end;

procedure TLineTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TLineFigure;
begin
 Setlength(CurrentFigures, length(CurrentFigures) + 1);
 CurrentFigures[high(CurrentFigures)] := TLine.Create();
 AFigure := (CurrentFigures[high(CurrentFigures)] as TLineFigure);
 SetLength(AFigure.Points, 2);
 AFigure.Points[0] := ScreenToWorld(Point(X,Y));
 AFigure.Points[1] := ScreenToWorld(Point(X,Y));
 AFigure.PenColor := APenColor;
 AFigure.Width := AWidth;
 AFigure.PenStyle := APenStyle;
 MaxMin(Point(X,Y));
end;

procedure TRectangleTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TObjectFigure;
begin
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
 MaxMin(Point(X,Y));
end;

procedure TEllipseTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TObjectFigure;
begin
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
 MaxMin(Point(X,Y));
end;
procedure TRoundedRectangleTool.MouseDown(X: integer;Y: integer);
var
  AFigure: TObjectFigure;
begin
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
  MaxMin(Point(X,Y));
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
