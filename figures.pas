unit Figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ActnList, Spin, GraphMath, Scale, TypInfo, Windows;

type
  TFigureClass = class of TFigure;
  tempPointsArray = array[0..3] of TPoint;
  PolygonPointsArray = array of TPoint;
  StringArray = array of string;

  TFigure = class
    Selected: boolean;
    Index: integer;
    Region: HRGN;
    Points: array of TFloatPoint;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure SetRegion; virtual; abstract;
    procedure DrawSelection(AFigure: TFigure; Canvas: TCanvas; Width: integer); virtual;
    function Save(AFigure: TFigure): StringArray; virtual;abstract;
 class procedure Download(n: integer; a: StringArray); virtual;abstract;
  end;

  TLineFigure = class(TFigure)
    PenColor, BrushColor: TColor;
    PenStyle: TPenStyle;
    Width: integer;
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function Save(AFigure: TFigure): StringArray; override;
 class  procedure Download(n: integer; a: StringArray);
  end;

  TObjectFigure = class(TLineFigure)
    BrushStyle: TBrushStyle;
    RoundingRadiusX: integer;
    RoundingRadiusY: integer;
    CurrentLineColor, CurrentFillColor: TColor;
    CurrentLineWidth: integer;
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function Save(AFigure: TFigure): StringArray; override;
 class  procedure Download(n: integer; a: StringArray);
  end;

  TPolyLine = class(TLineFigure)
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function Save(AFigure: TFigure): StringArray; override;
 class  procedure Download(n: integer; a: StringArray);
  end;

  TLine = class(TLineFigure)
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function Save(AFigure: TFigure): StringArray; override;
 class  procedure Download(n: integer; a: StringArray);
  end;

  TEllipce = class(TObjectFigure)
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function Save(AFigure: TFigure): StringArray; override;
 class  procedure Download(n: integer; a: StringArray);
  end;

  TRectangle = class(TObjectFigure)
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function Save(AFigure: TFigure): StringArray; override;
 class  procedure Download(n: integer; a: StringArray);
  end;

  TRectangleMagnifier = class(TLineFigure)
    BrushStyle: TBrushStyle;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TRoundedRectangle = class(TObjectFigure)
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function Save(AFigure: TFigure): StringArray; override;
 class  procedure Download(n: integer; a: StringArray);
  end;
   Figures1 =  array of TFigure;
procedure LineRegion(p1, p2: TPoint; var tempPoints: array of TPoint; Width: integer);
procedure CreateArrayOfActions();
procedure RefreshFigures(N: integer);
function RefreshArrays(B: Figures1): Figures1;
procedure StopUndo();
var
  CurrentFigures: array of TFigure;
  layer: array of TFigure;
  Buffer: array of TFigure;
  Now: Integer;
  UndoFlag: Boolean;
  ArrayOfActions: array of Figures1;
  const BStyles: array [0..7] of TBrushStyle = (bsClear, bsSolid,
  bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
    PStyles: array[0..5] of TPenStyle = (psSolid, psClear, psDot,
    psDash, psDashDot, psDashDotDot);
implementation

procedure CreateArrayOfActions();
begin
  SetLength(ArrayOfActions, Length(ArrayOfActions) + 1);
  SetLength(ArrayOfActions[High(ArrayOfActions)], Length(CurrentFigures));
  ArrayOfActions[High(ArrayOfActions)] := RefreshArrays(CurrentFigures);
  Now := Now + 1;
end;

procedure RefreshFigures(N: integer);
begin
  SetLength(CurrentFigures, 0);
  SetLength(CurrentFigures, Length(ArrayOfActions[N]));
  CurrentFigures := RefreshArrays(ArrayOfActions[N]);
end;

function RefreshArrays(B: Figures1): Figures1;
var
  a: Figures1;
  i, q: integer;
begin
  SetLength(A, Length(B));
  for i := 0 to high(B) do begin
  case B[i].ClassName of

    'TPolyLine':         A[i] := TPolyLine.Create;
    'TLine'    :         A[i] := TLine.Create;
    'TEllipce' :         begin
                           A[i] := TEllipce.Create;
                           (A[i]as TObjectFigure).BrushColor := (B[i] as TObjectFigure).BrushColor;
                           (A[i] as TObjectFigure).BrushStyle := (B[i] as TObjectFigure).BrushStyle;
                         end;
    'TRectangle':        begin
                           A[i] := TRectangle.Create;
                           (A[i] as TObjectFigure).BrushColor := (B[i] as TObjectFigure).BrushColor;
                           (A[i] as TObjectFigure).BrushStyle := (B[i] as TObjectFigure).BrushStyle;
                         end;
    'TRoundedRectangle': begin
                           A[i] := TRoundedRectangle.Create;
                           (A[i]as TObjectFigure).BrushColor := (B[i] as TObjectFigure).BrushColor;
                           (A[i] as TObjectFigure).BrushStyle := (B[i] as TObjectFigure).BrushStyle;
                           (A[i] as TRoundedRectangle).RoundingRadiusX := (B[i] as TRoundedRectangle).RoundingRadiusX;
                           (A[i] as TRoundedRectangle).RoundingRadiusY := (B[i] as TRoundedRectangle).RoundingRadiusY;
                         end;
  end;
  SetLength(A[i].Points, Length(B[i].Points));
  for q := 0 to Length(B[i].Points) do A[i].Points[q] := B[i].Points[q];

  (A[i] as TLineFigure).PenColor := (B[i] as TLineFigure).PenColor;
  (A[i] as TLineFigure).PenStyle := (B[i] as TLineFigure).PenStyle;
  (A[i] as TLineFigure).Width := (B[i] as TLineFigure).Width;
end;
  Result := A;
end;

procedure StopUndo();
begin
  SetLength(ArrayOfActions, Now + 1);
  UndoFlag := False;
end;
procedure TLineFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := PenColor;
  ACanvas.Pen.Style := PenStyle;
  ACanvas.Pen.Width := Width;
end;

procedure TObjectFigure.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Brush.Style := BrushStyle;
  ACanvas.Brush.Color := BrushColor;
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  i: integer;
begin
  inherited;
  for i := 1 to high(Points) - 1 do
    ACanvas.Line(WorldToScreen(Points[i]), WorldToScreen(Points[i + 1]));
end;

procedure TEllipce.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TRectangleMagnifier.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Frame(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TRoundedRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y,
    RoundingRadiusX, RoundingRadiusY);
end;

procedure TObjectFigure.SetRegion;
begin
end;

procedure TLineFigure.SetRegion;
begin
end;

procedure TRectangle.SetRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateRectRgn(RegionRect.Left, RegionRect.Top,
    RegionRect.Right, RegionRect.Bottom);
end;

procedure TEllipce.SetRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateEllipticRgn(RegionRect.Left, RegionRect.Top,
    RegionRect.Right, RegionRect.Bottom);
end;

procedure TRoundedRectangle.SetRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateRoundRectRgn(RegionRect.Left, RegionRect.Top, RegionRect.Right,
    RegionRect.Bottom, RoundingRadiusX, RoundingRadiusY);
end;

procedure TLine.SetRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1, p2: TPoint;
begin
  p1 := WorldToScreen(Points[0]);
  p2 := WorldToScreen(Points[1]);
  LineRegion(p1, p2, RegionPoints, Width);
  Region := CreatePolygonRgn(RegionPoints, 3, 2);
end;

procedure TPolyline.SetRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1, p2: TPoint;
  curRgn: HRGN;
  i: integer;
begin
  for i := 0 to high(Points) - 1 do
  begin
    p1 := WorldToScreen(Points[i]);
    p2 := WorldToScreen(Points[i + 1]);
    LineRegion(p1, p2, RegionPoints, Width);
    if (i = low(Points)) then
      Region := CreatePolygonRgn(RegionPoints, 3, 2);
    curRgn := CreatePolygonRgn(RegionPoints, 3, 2);
    CombineRgn(Region, Region, curRgn, RGN_OR);
    DeleteObject(curRgn);
  end;
end;

procedure LineRegion(p1, p2: TPoint; var tempPoints: array of TPoint; Width: integer);
begin
  if (abs(p2.x - p1.x) > 45) then
  begin
    tempPoints[0].x := p1.x - Width div 2;
    tempPoints[0].y := p1.y - 5 - Width;
    tempPoints[1].x := p2.x + Width div 2;
    tempPoints[1].y := p2.y - 5 - Width;
    tempPoints[2].x := p2.x + Width div 2;
    tempPoints[2].y := p2.y + 5 + Width;
    tempPoints[3].x := p1.x - Width div 2;
    tempPoints[3].y := p1.y + 5 + Width;
  end
  else
  begin
    tempPoints[0].x := p1.x - 5 - Width;
    tempPoints[0].y := p1.y - Width div 2;
    tempPoints[1].x := p2.x - 5 - Width;
    tempPoints[1].y := p2.y + Width div 2;
    tempPoints[2].x := p2.x + 5 + Width;
    tempPoints[2].y := p2.y + Width div 2;
    tempPoints[3].x := p1.x + 5 + Width;
    tempPoints[3].y := p1.y - Width div 2;
  end;
end;

procedure TFigure.DrawSelection(AFigure: TFigure; Canvas: TCanvas; Width: integer);
var
  Point1, Point2, a:TFloatPoint;
  i: integer;
begin
If length(AFigure.Points) = 2 then begin
   Point1.X := AFigure.Points[0].X;
   Point1.Y := AFigure.Points[0].Y;
   Point2.X := AFigure.Points[1].X;
   Point2.Y := AFigure.Points[1].Y;

  if (Point1.X>Point2.X) then
    begin
      a.X:=Point1.X;
      Point1.X:=Point2.X;
      Point2.X:=a.X;
    end;
  if (Point1.Y>Point2.Y) then
    begin
      a.Y:=Point1.Y;
      Point1.Y:=Point2.Y;
      Point2.Y:=a.Y;
    end;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDash;
  Canvas.Brush.Color := clBlack;
  Canvas.Frame(WorldToScreen(Point1).x - 5 - Width div 2,WorldToScreen(Point1).y - 5 - Width div 2,
               WorldToScreen(Point2).x + 5 + Width div 2,WorldToScreen(Point2).y + 5 + Width div 2);

  Canvas.Rectangle(WorldToScreen(Point1).x - 15 - Width div 2,
                   WorldToScreen(Point1).y - 15 - Width div 2,
                   WorldToScreen(Point1).x - 5 - Width div 2,
                   WorldToScreen(Point1).y - 5 - width div 2);

  Canvas.Rectangle(WorldToScreen(Point2).x + 15 - Width div 2,
                   WorldToScreen(Point2).y + 15 - Width div 2,
                   WorldToScreen(Point2).x + 5 - Width div 2,
                   WorldToScreen(Point2).y + 5 - width div 2);

  Canvas.Rectangle(WorldToScreen(Point2).x + 15 - Width div 2,
                   WorldToScreen(Point1).y - 15 - Width div 2,
                   WorldToScreen(Point2).x + 5 - Width div 2,
                   WorldToScreen(Point1).y - 5 - width div 2);

  Canvas.Rectangle(WorldToScreen(Point1).x - 15 - Width div 2,
                   WorldToScreen(Point2).y + 15 - Width div 2,
                   WorldToScreen(Point1).x - 5 - Width div 2,
                   WorldToScreen(Point2).y + 5 - width div 2);



 end else begin
   for i:=0 to high(AFigure.points) do begin
     Canvas.Pen.Color := clBlack;
     Canvas.Pen.Width := 1;
     Canvas.Pen.Style := psDash;
     Canvas.Brush.Color := clBlack;
     Canvas.Rectangle(WorldToScreen((AFigure as TPolyLine).points[i]).x - 5 - Width div 2,
                     WorldToScreen((AFigure as TPolyLine).points[i]).y - 5 - Width div 2,
                     WorldToScreen((AFigure as TPolyLine).points[i]).x + 5 - Width div 2,
                     WorldToScreen((AFigure as TPolyLine).points[i]).y + 5 - width div 2);
   end;
 end;
end;
function TLineFigure.Save(AFigure: TFigure): StringArray;
begin
  SetLength(Save, 7);
  Save[0] := FloatToStr((AFigure as TLineFigure).Points[0].X);
  Save[1] := FloatToStr((AFigure as TLineFigure).Points[0].Y);
  Save[2] := FloatToStr((AFigure as TLineFigure).Points[1].X);
  Save[3] := FloatToStr((AFigure as TLineFigure).Points[1].Y);
  Save[4] := IntToStr((AFigure as TLineFigure).Width);
  Save[5] := IntToStr(ord((AFigure as TLineFigure).PenStyle));
  Save[6] := ColorToString((AFigure as TLineFigure).PenColor);
end;

function TObjectFigure.Save(AFigure: TFigure): StringArray;
begin
  Save:=Inherited Save(AFigure);
  SetLength(Save, Length(Save) + 2);
  Save[High(Save) - 1] := IntToStr(ord((AFigure as TObjectFigure).BrushStyle));
  Save[High(Save)] :=  ColorToString((AFigure as TObjectFigure).BrushColor);
end;

function TRectangle.Save(AFigure: TFigure): StringArray;
begin
  Save:=Inherited Save(AFigure);
end;

function TEllipce.Save(AFigure: TFigure): StringArray;
begin
  Save:=Inherited Save(AFigure);
end;

function TLine.Save(AFigure: TFigure): StringArray;
begin
  Save:=Inherited Save(AFigure);
end;

function TPolyline.Save(AFigure: TFigure): StringArray;
var
  i, j: integer;
begin
  SetLength(Save, 2 * Length(AFigure.Points) + 3);
  Save[0]:=IntToStr(Length(AFigure.Points));
  Save[1]:=IntToStr((AFigure as TLineFigure).Width);
  Save[3] := IntToStr(ord((AFigure as TLineFigure).PenStyle));
  Save[4]:= ColorToString((AFigure as TLineFigure).PenColor);
  i := 4;
  for j:=0 to high(AFigure.Points)do begin
    Save[i]:=FloatToStr(AFigure.Points[j].x);
    Save[i + 1]:=FloatToStr(AFigure.Points[j].y);
    i := i + 2;
  end;
end;

function TRoundedRectangle.Save(AFigure: TFigure): StringArray;
begin
  Save:=Inherited Save(AFigure);
  SetLength(Save, Length(Save) + 2);
  Save[high(Save) - 1]:=IntToStr((AFigure as TRoundedRectangle).RoundingRadiusX);
  Save[high(Save)]:=IntToStr(((AFigure as TRoundedRectangle).RoundingRadiusY));
end;

class procedure TLineFigure.Download(n: integer; a: StringArray);
begin
  SetLength(CurrentFigures[n].Points, 2);
  CurrentFigures[n].Points[0].X := StrToFloat(a[0]);
  CurrentFigures[n].Points[0].Y := StrToFloat(a[1]);
  CurrentFigures[n].Points[1].X := StrToFloat(a[2]);
  CurrentFigures[n].Points[1].Y := StrToFloat(a[3]);
 (CurrentFigures[n] as TLineFigure).Width := StrToInt(a[4]);
 (CurrentFigures[n] as TLineFigure).PenStyle := PStyles[StrToInt(a[5])];
 (CurrentFigures[n] as TLineFigure).PenColor := StringToColor(a[6]);
end;

class procedure TObjectFigure.Download(n: integer; a: StringArray);
begin
  Inherited Download(n,a);
  (CurrentFigures[n] as TObjectFigure).BrushStyle := BStyles[StrToInt(a[7])];
  (CurrentFigures[n] as TObjectFigure).BrushColor := StringToColor(a[8]);
end;

class procedure TRectangle.Download(n: integer; a: StringArray);
begin
  CurrentFigures[n] := TRectangle.Create();
  Inherited Download(n,a);
end;

class procedure TEllipce.Download(n: integer; a: StringArray);
begin
  CurrentFigures[n] := TEllipce.Create();
  Inherited Download(n,a);
end;

class procedure TRoundedRectangle.Download(n: integer; a: StringArray);
begin
  CurrentFigures[n] := TRoundedRectangle.Create;
  Inherited Download(n,a);
  (CurrentFigures[n] as TRoundedRectangle).RoundingRadiusX := StrToInt(a[9]);
  (CurrentFigures[n] as TRoundedRectangle).RoundingRadiusY := StrToInt(a[10]);
end;

class procedure TLine.Download(n: integer; a: StringArray);
begin
  CurrentFigures[n] := TLine.Create;
  Inherited Download(n,a);
end;

class procedure TPolyLine.Download(n: integer; a: StringArray);
var
   i, j: integer;
begin
  CurrentFigures[n] := TPolyLine.Create();
  SetLength(CurrentFigures[n].Points, StrToInt(a[0]));
  (CurrentFigures[n] as TPolyLine).Width := StrToInt(a[1]);
  (CurrentFigures[n] as TLineFigure).PenStyle := PStyles[StrToInt(a[2])];
  (CurrentFigures[n] as TPolyLine).PenColor := StringToColor(a[3]);
  i := 4;
  for j:=0 to (strtoint(a[0])) * 2 - 1 do begin
    CurrentFigures[n].Points.[j].x:=StrToFloat(a[i]);
    CurrentFigures[n].Points.[j].y:=StrToFloat(a[i + 1]);
    i:= i + 2;
  end;
end;
procedure SetOffset(APoint: TFloatPoint);
begin
  Offset := APoint;
end;
end.
begin
  Now := 0;
end
