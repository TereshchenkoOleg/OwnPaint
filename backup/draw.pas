unit Draw;

{$mode objfpc}{$H+}


interface

uses
 Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, Help, Figures, tools;

type

  { TDForm }
  TDForm = class(TForm)
    ChangeFillColorButton: TColorButton;
    ToolPanel: TPanel;
    ChangeLineColorButton: TColorButton;
    ColorDialog: TColorDialog;
    EditorActionList1: TActionList;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    CloseItem: TMenuItem;
    HelpItem: TMenuItem;
    PaintBox: TPaintBox;
    Panel: TPanel;
    PenWidth: TSpinEdit;
    BackAction: TAction;
    BackButton: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure BackButtonChangeBounds(Sender: TObject);
    procedure HelpItemClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);
    procedure CloseItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PenWidthChange(Sender: TObject);
    procedure ChangeLineColorButtonColorChanged(Sender: TObject);
    procedure ChangeFillColorButtonColorChanged(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure BackActionExecute(Sender: TObject);
    procedure PenWidthChangeBounds(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
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

const
  TOOL_BUTTON_SIZE = 32;
  TOOL_BUTTON_MARGIN = 2;
  TOOL_BUTTON_PADDING = 1;

var
  DForm: TDForm;
  IsDrawing: boolean;
  CurrentFigure: TFigureClass;
  CanvasItems, History: array of TFigure;
  CurrentLineColor, CurrentFillColor: TColor;
  CurrentLineWidth, CurrentFillStyle: integer;

implementation

{$R *.lfm}

{ TDForm }
procedure TDForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  CurrentIcon: TPicture;
  IconsPerRow: integer;
begin
  IsDrawing := False;
  DForm.DoubleBuffered := True;
  DForm.Caption := ApplicationName;

  CurrentLineColor := clBlack;
  ChangeLineColorButton.ButtonColor := CurrentLineColor;

  CurrentFillColor := clWhite;
  ChangeFillColorButton.ButtonColor := CurrentFillColor;

  CurrentLineWidth := 1;
  PenWidth.Value := CurrentLineWidth;

  IconsPerRow := ToolPanel.Width div (TOOL_BUTTON_SIZE +
    TOOL_BUTTON_MARGIN + TOOL_BUTTON_PADDING);

  ToolPanel.Height := (Length(FiguresBase) div IconsPerRow) *
    (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) + TOOL_BUTTON_PADDING * 2;
  for i := Low(FiguresBase) to High(FiguresBase) do
  begin
    b := TSpeedButton.Create(ToolPanel);
    b.Transparent := True;
    b.Parent := ToolPanel;
    b.Name := 'ToolButton' + FiguresBase[i].ClassName;
    b.Tag := i;
    b.OnClick := @ToolButtonClick;

    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile(FiguresBase[i].ClassName + '.png');
    b.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;

    b.Left := (i mod IconsPerRow) * (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) +
      TOOL_BUTTON_PADDING;
    b.Top := (i div IconsPerRow) * (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) +
      TOOL_BUTTON_PADDING;
    b.Width := TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN;
    b.Height := b.Width;
    if i = 0 then
      b.Click;
  end;

end;

procedure TDForm.ToolButtonClick(Sender: TObject);
var
  b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  CurrentFigure := FiguresBase[b.Tag];
end;

procedure TDForm.ChangeLineColorButtonColorChanged(Sender: TObject);
begin
  CurrentLineColor := ChangeLineColorButton.ButtonColor;
end;

procedure TDForm.ChangeFillColorButtonColorChanged(Sender: TObject);
begin
  CurrentFillColor := ChangeFillColorButton.ButtonColor;
end;

procedure TDForm.PenWidthChange(Sender: TObject);
begin
  CurrentLineWidth := PenWidth.Value;
end;

procedure TDForm.CloseItemClick(Sender: TObject);
begin
  DForm.Close;
end;

procedure TDForm.HelpItemClick(Sender: TObject);
begin
  HelpForm.ShowModal;
end;

procedure TDForm.BackButtonChangeBounds(Sender: TObject);
begin

end;

procedure TDForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    IsDrawing := True;
    SetLength(CanvasItems, Length(CanvasItems) + 1);
    CanvasItems[High(CanvasItems)] :=
      CurrentFigure.Create(X, Y, CurrentLineColor, CurrentLineWidth,
      CurrentFillColor);
  end;
  PaintBox.Invalidate;
end;

procedure TDForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if IsDrawing then
  begin
    CanvasItems[High(CanvasItems)].MouseMove(X, Y);
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
    PaintBox.Invalidate;
    for i := Low(History) to High(History) do
      FreeAndNil(History[i]);
    SetLength(History, 0);
  end;
end;

procedure TDForm.PaintBoxPaint(Sender: TObject);
var
  i: TFigure;
begin
  with PaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  end;
  for i in CanvasItems do
    i.Draw(PaintBox.Canvas);
end;

procedure TDForm.BackActionExecute(Sender: TObject);
begin
  if Length(CanvasItems) > 0 then
  begin
    SetLength(History, Length(History) + 1);
    History[High(History)] := CanvasItems[High(CanvasItems)];
    SetLength(CanvasItems, Length(CanvasItems) - 1);
    PaintBox.Invalidate;
  end;
end;

procedure TDForm.PenWidthChangeBounds(Sender: TObject);
begin

end;

end.
