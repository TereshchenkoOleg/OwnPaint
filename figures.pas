unit Figures;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ActnList, Spin, GraphMath, Scale;

type
TFigure = class
  procedure Draw(ACanvas:TCanvas); virtual;abstract;
end;

TLineFigure = class(TFigure)
  Points: array of TFloatPoint;
  PenColor,BrushColor: TColor;
  Width: integer;
  procedure Draw(ACanvas:TCanvas); override;
end;

TObjectFigure = class(TLineFigure)
  BrushStyle: TBrushStyle;
  RoundingRadiusX: integer;
  RoundingRadiusY:Integer;
  CurrentLineColor, CurrentFillColor: TColor;
  CurrentLineWidth: integer;
  procedure Draw(ACanvas:TCanvas); override;
end;

TPolyLine = class(TLineFigure)
  procedure Draw(ACanvas:TCanvas); override;
end;

TLine = class(TLineFigure)
  procedure Draw(ACanvas:TCanvas); override;
end;

TEllipce = class(TObjectFigure)
  procedure Draw(ACanvas:TCanvas); override;
end;

TRectangle = class(TObjectFigure)
  procedure Draw(ACanvas:TCanvas); override;
end;

TRectangleMagnifier= class(TLineFigure)
  BrushStyle: TBrushStyle;
  procedure Draw(ACanvas:TCanvas); override;
end;
TRoundedRectangle= class(TObjectFigure)
  procedure Draw(ACanvas:TCanvas); override;
  end;
var
  CurrentFigures: array of TFigure;
  layer: array of Tfigure;

Implementation

procedure TLineFigure.Draw(ACanvas:TCanvas);
begin
  ACanvas.Pen.Color := PenColor;
  ACanvas.Pen.Width := Width;
end;

procedure TObjectFigure.Draw(ACanvas:TCanvas);
begin
  Inherited;
   ACanvas.Brush.Style := BrushStyle;
  ACanvas.Brush.Color := BrushColor;
end;

procedure TLine.Draw(ACanvas:TCanvas);
begin
  Inherited;
  ACanvas.Line(WorldToScreen(Points[0]).x,WorldToScreen(Points[0]).Y,WorldToScreen(Points[1]).x,WorldToScreen(Points[1]).Y);
end;

procedure TPolyLine.Draw(ACanvas:TCanvas);
var
i: integer;
begin
  Inherited;
  for i:=1 to high(Points)-1 do
  ACanvas.Line(WorldToScreen(Points[i]), WorldToScreen(Points[i+1]));
end;

procedure TEllipce.Draw(ACanvas:TCanvas);
begin
  Inherited;
  ACanvas.Ellipse(WorldToScreen(Points[0]).x,WorldToScreen(Points[0]).Y,WorldToScreen(Points[1]).x,WorldToScreen(Points[1]).Y);
end;

procedure TRectangle.Draw(ACanvas:TCanvas);
begin
  Inherited;
  ACanvas.Rectangle(WorldToScreen(Points[0]).x,WorldToScreen(Points[0]).Y,WorldToScreen(Points[1]).x,WorldToScreen(Points[1]).Y);
end;

procedure TRectangleMagnifier.Draw(ACanvas:TCanvas);
begin
  Inherited;
  ACanvas.Frame(WorldToScreen(Points[0]).x,WorldToScreen(Points[0]).Y,WorldToScreen(Points[1]).x,WorldToScreen(Points[1]).Y);
end;

procedure TRoundedRectangle.Draw(ACanvas:TCanvas);
begin
  Inherited;
  ACanvas.RoundRect(WorldToScreen(Points[0]).x,WorldToScreen(Points[0]).Y,WorldToScreen(Points[1]).x,WorldToScreen(Points[1]).Y, RoundingRadiusX,RoundingRadiusY);
end;
procedure SetOffset (APoint: TFloatPoint);
begin
  Offset := APoint;
end;
end.
