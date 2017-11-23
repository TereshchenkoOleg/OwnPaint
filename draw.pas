unit Draw;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, GraphMath, Dialogs, Menus,
  ExtCtrls, LCLIntf, LCLType, Spin, ActnList, Buttons, StdCtrls, Help,
  Figures, tools, Scale, toolsparams;

type

  { TDForm }
  TDForm = class(TForm)
    ClearAll: TSpeedButton;
    ScrollBarHorizontal: TScrollBar;
    ShowAllButton: TSpeedButton;
    ZoomSpinEdit: TSpinEdit;
    ScrollBarVertical: TScrollBar;
    ToolPanel: TPanel;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    CloseItem: TMenuItem;
    HelpItem: TMenuItem;
    PaintBox: TPaintBox;
    Panel: TPanel;
    BackButton: TSpeedButton;
    procedure ClearAllClick(Sender: TObject);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure HelpItemClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CloseItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure BackActionExecute(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure ZoomSpinEditChange(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

  TPolyLine = record
    Vert: array of TPoint;
    Color: TColor;
    Width: integer;
  end;

var
  DForm: TDForm;
  IsDrawing: boolean;
  CurrentTool: TFigureTool;
  Param: array of TParam;
  CanvasItems, History: array of TFigure;
implementation

{$R *.lfm}

{ TDForm }
procedure TDForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  CurrentIcon: TPicture;
begin
  IsDrawing := False;
  DForm.DoubleBuffered := True;
  DForm.Caption := ApplicationName;
  CurrentTool := TPolyLineTool.Create();
  AWidth := 1;
  ARadiusX := 30;
  ARadiusY := 30;
  Zoom := 100;
  for i := Low(Tool) to High(Tool) do
  begin
    b := TSpeedButton.Create(ToolPanel);
    b.Parent := ToolPanel;
    b.Name := Tool[i].ClassName;
    b.Width := 32;
    b.Height := 32;
    b.Top := (i div 3) * 32;
    b.Left := (i mod 3) * 32;
    b.GroupIndex := 1;
    b.Tag := i;
    b.OnClick := @ToolButtonClick;
    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile('./icons/' + Tool[i].ClassName + '.png');
    b.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;
  end;
end;

procedure TDForm.ToolButtonClick(Sender: TObject);
var
ParamsPanel: TPanel;
begin
  CurrentTool := Tool[(Sender as TSpeedButton).tag];
  ParamsPanel:= TPanel.Create(DForm);
  ParamsPanel.Parent:=Panel;
  ParamsPanel.Width:=110;
  ParamsPanel.Height:=300;
  ParamsPanel.Left:=8;
  ParamsPanel.Top:=248;
  CurrentTool.ParamsCreate(ParamsPanel);
  Invalidate;
end;

procedure TDForm.CloseItemClick(Sender: TObject);
begin
  DForm.Close;
end;

procedure TDForm.HelpItemClick(Sender: TObject);
begin
  HelpForm.ShowModal;
end;

procedure TDForm.ShowAllButtonClick(Sender: TObject);
begin
  RectZoom(PaintBox.Height, PaintBox.Width, MinPoint, MaxPoint);
  Invalidate;
  ScrollBarVertical.Max := trunc(MaxPoint.Y);
  ScrollBarVertical.Min := trunc(MinPoint.Y);
  ScrollBarHorizontal.Max := trunc(MaxPoint.X);
  ScrollBarHorizontal.Min := trunc(MinPoint.X);
  Offset.X := 0;
  Offset.Y := 0;
end;

procedure TDForm.ClearAllClick(Sender: TObject);
begin
  SetLength(CurrentFigures, 0);
  PaintBox.Invalidate;
end;

procedure TDForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    IsDrawing := True;
    CurrentTool.MouseDown(X, Y, AWidth);
    MaxMin(ScreenToWorld(Point(X, Y)));
    PaintBox.Invalidate;
  end;
end;

procedure TDForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if IsDrawing then
  begin
    CurrentTool.MouseMove(X, Y);
    MaxMin(ScreenToWorld(Point(X, Y)));
    PaintBox.Invalidate;
  end;
end;

procedure TDForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  if Button = mbLeft then
  begin
    IsDrawing := False;
    CurrentTool.MouseUp(X, Y, PaintBox.Canvas);
    PaintBox.Invalidate;
  end;
end;

procedure TDForm.PaintBoxPaint(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(CurrentFigures) do
  begin
    CurrentFigures[i].Draw(PaintBox.Canvas);
  end;
  ScrollBarVertical.Max := trunc(MaxPoint.Y);
  ScrollBarVertical.Min := trunc(MinPoint.Y);
  ScrollBarHorizontal.Max := trunc(MaxPoint.X);
  ScrollBarHorizontal.Min := trunc(MinPoint.X);
  ZoomSpinEdit.Value := Zoom;
  AHeightPB := PaintBox.Height;
  AWidthPB := PaintBox.Width;
end;

procedure TDForm.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  Offset := Point(ScrollBarHorizontal.Position, ScrollBarVertical.Position);
  PaintBox.Invalidate;
end;

procedure TDForm.ZoomSpinEditChange(Sender: TObject);
begin
  ZoomSpinEdit.Value := Zoom;
  Invalidate;
end;

procedure TDForm.BackActionExecute(Sender: TObject);
begin
  if Length(CurrentFigures) > 0 then
  begin
    SetLength(CurrentFigures, Length(CurrentFigures) - 1);
    PaintBox.Invalidate;
  end;
end;

end.
