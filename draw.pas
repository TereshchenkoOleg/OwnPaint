unit Draw;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, GraphMath, Dialogs, Menus,
  ExtCtrls, LCLIntf, LCLType, Spin, ActnList, Buttons, StdCtrls, Help,
  Figures, tools, Scale;

type

  { TDForm }
  TDForm = class(TForm)
    RedoItem: TMenuItem;
    EditItem: TMenuItem;
    ClearItem: TMenuItem;
    DeleteSelectedItem: TMenuItem;
    CopyItem: TMenuItem;
    UpItem: TMenuItem;
    DownItem: TMenuItem;
    Saveitem: TMenuItem;
    OpenItem: TMenuItem;
    PasteItem: TMenuItem;
    UndoItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SelectUpItem: TMenuItem;
    SelectDownItem: TMenuItem;
    SelectedAllItem: TMenuItem;
    ScrollBarHorizontal: TScrollBar;
    ShowItem: TMenuItem;
    ZoomSpinEdit: TSpinEdit;
    ScrollBarVertical: TScrollBar;
    ToolPanel: TPanel;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    CloseItem: TMenuItem;
    HelpItem: TMenuItem;
    PaintBox: TPaintBox;
    Panel: TPanel;
    procedure ClearAllClick(Sender: TObject);
    procedure DeleteSelectedItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SelectUpItemClick(Sender: TObject);
    procedure SelectDownItemClick(Sender: TObject);
    procedure SelectedAllItemClick(Sender: TObject);
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
    procedure SaveButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure RedoClick(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure PasteSelectedClick(Sender: TObject);
    procedure CopySelectedClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;
  TRecPolyLine = record
    Vert: array of TPoint;
    Color: TColor;
    Width: integer;
  end;
procedure SavePicture(PName: string);
procedure OpenPicture(PName: string);
const
  Sign: string = 'OwnPaint';
var
  DForm: TDForm;
  IsDrawing, BufferFlag: boolean;
  CurrentTool, TPawTool: TFigureTool;
  Param: array of TParam;
  CanvasItems, History: array of TFigure;
  CurrentPicture: string;
implementation

{$R *.lfm}

{ TDForm }
procedure TDForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  CurrentIcon: TPicture;
begin
  zoom:=1;
  IsDrawing := False;
  DForm.DoubleBuffered := True;
  DForm.Caption := ApplicationName;
  CurrentTool := TPolyLineTool.Create();
  AWidth := 1;
  ARadiusX := 30;
  ARadiusY := 30;
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
  i: integer;
begin
  CurrentTool := Tool[(Sender as TSpeedButton).tag];
  ParamsPanel := TPanel.Create(DForm);
  ParamsPanel.Parent := Panel;
  ParamsPanel.Width := 110;
  ParamsPanel.Height := 300;
  ParamsPanel.Left := 8;
  ParamsPanel.Top := 248;
  CurrentTool.ParamsCreate(ParamsPanel);
    if not ((Sender as TSpeedbutton).tag = 8) then
      for i := 0 to High(CurrentFigures) do CurrentFigures[i].Selected := False;
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

procedure TDForm.DeleteSelectedItemClick(Sender: TObject);
var
  i, j: integer;
begin
  j := 0;
  for i := 0 to high(CurrentFigures) do
  begin
    if (CurrentFigures[i].Selected) then
      FreeAndNil(CurrentFigures[i])
    else
    begin
      CurrentFigures[j] := CurrentFigures[i];
      j := j + 1;
    end;
  end;
  setLength(CurrentFigures, j);
  Invalidate;
end;

procedure TDForm.OpenItemClick(Sender: TObject);
begin

end;

procedure TDForm.SaveItemClick(Sender: TObject);
begin

end;

procedure TDForm.SelectDownItemClick(Sender: TObject);
var
  i, j, k: Integer;
  Figure: TFigure;
begin
  k := 0;
  for i := high(CurrentFigures) downto 0 do
    begin
      if (CurrentFigures[i].Selected) then
        begin
          for j := i downto k + 1  do
          begin
            Figure := CurrentFigures[j];
            CurrentFigures[j] := CurrentFigures[j-1];
            CurrentFigures[j-1] := Figure;
            k := j
          end;
        end;
    end;
  Invalidate;
end;

procedure TDForm.SelectUpItemClick(Sender: TObject);
var
  i, j, k: integer;
  Figure: TFigure;
begin
  k := high(CurrentFigures);
  for i := 0 to high(CurrentFigures) do
  begin
    if (CurrentFigures[i].Selected) then
    begin
      for j := i to k - 1 do
      begin
        Figure := CurrentFigures[j];
        CurrentFigures[j] := CurrentFigures[j + 1];
        CurrentFigures[j + 1] := Figure;
        k := j;
      end;
    end;
  end;
  Invalidate;
end;


procedure TDForm.SelectedAllItemClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(CurrentFigures) do
    CurrentFigures[i].Selected := True;
  Invalidate;
end;

procedure TDForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    IsDrawing := True;
    CurrentTool.MouseDown(X, Y);
    MaxMin(ScreenToWorld(Point(X, Y)));
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
  ParamsPanel: TPanel;
  i: integer;
begin
  if Button = mbLeft then
  begin
    IsDrawing := False;
    CurrentTool.MouseUp(X, Y, PaintBox.Canvas);
    if SelectedCreateParamFlag then begin
  ParamsPanel := TPanel.Create(DForm);
  ParamsPanel.Parent := Panel;
  ParamsPanel.Width := 110;
  ParamsPanel.Height := 300;
  ParamsPanel.Left := 8;
  ParamsPanel.Top := 248;
  //SelectedFigure.ParamsCreate(ParamsPanel);
  end;
  SelectedCreateParamFlag := False;
  PaintBox.Invalidate;
  end;
end;

procedure TDForm.PaintBoxPaint(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(CurrentFigures) do
  begin
    CurrentFigures[i].Draw(PaintBox.Canvas);
    if CurrentFigures[i].Selected then
      CurrentFigures[i].DrawSelection(CurrentFigures[i], PaintBox.Canvas,
        (CurrentFigures[I] as TLineFigure).Width);
  end;
  ScrollBarVertical.Max := trunc(MaxPoint.Y);
  ScrollBarVertical.Min := trunc(MinPoint.Y);
  ScrollBarHorizontal.Max := trunc(MaxPoint.X);
  ScrollBarHorizontal.Min := trunc(MinPoint.X);
  ZoomSpinEdit.Value := Zoom;
  AHeightPB := PaintBox.Height;
  AWidthPB := PaintBox.Width;
end;

procedure TDForm.BackActionExecute(Sender: TObject);
begin

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
procedure TDForm.OpenButtonClick(Sender: TObject);
begin
if OpenDialog.Execute then begin
    OpenPicture(OpenDialog.Filename);
end;
Invalidate;
end;

procedure OpenPicture(PName: string);
var
  Picture: text;
  i, j: integer;
  s, l: String;
  a: StringArray;
begin
  assign(Picture, PName);
  reset(Picture);
  readln(Picture, s);
  if s = Sign then begin
    CurrentPicture := PName;
    readln(Picture, l);
    SetLength(CurrentFigures, StrToInt(l) + 1);
    for i := 0 to StrToInt(l) do  begin
      readln(Picture, s);
      readln(Picture);
      if (s = 'TPolyLine') then  begin
        setlength(a, 4);
        for j:=0 to 3 do readln(Picture, a[j]);

        if (s = 'TLine') then begin
          SetLength(a, 7);
          for j:=4 to 6 do readln(Picture, a[j]);
          TLine.Download(i, a) ;
          end;

        if (s = 'TRectangle') then begin
          SetLength(a, 9);
          for j:=4 to 8 do readln(Picture, a[j]);
          TRectangle.Download(i,a);
          end;

        if (s = 'TEllipce') then begin
          SetLength(a, 9);
          for j:=4 to 8 do readln(Picture, a[j]);
          TEllipce.Download(i, a);
          end;

        if (s = 'TRoundedRectangle') then begin
          SetLength(a, 11);
          for j:=4 to 10 do readln(Picture, a[j]);
          TRoundedRectangle.Download(i, a);
        end;

    end
    else begin
      readln(Picture);
      read(Picture, s);
      SetLength(a, StrToInt(s) + 2);
      a[0] := S;
      for j := 1 to high(a) do readln(Picture, a[j]);
      TPolyline.Download(i, a);
      readln(Picture);
    end;
    ReadLn(Picture);
    end;
  end;
end;
procedure SavePicture(PName: string);
var
  Picture: text;
  a: array of string;
  i, j: integer;
begin
  assign(Picture, PName);
  rewrite(Picture);
  writeln(Picture, Sign);
  writeln(Picture, high(CurrentFigures));
  for i:=0 to High(CurrentFigures) do begin
    writeln(Picture, CurrentFigures[i].ClassName);
    writeln(Picture, '{');
    a := CurrentFigures[i].Save
    (CurrentFigures[i]);
    for j:=0 to high(a) do writeln(Picture, a[j]);
    writeln(Picture, '}');
  end;
  CloseFile(Picture);
end;
procedure TDForm.SaveButtonClick(Sender: TObject);
begin
   if SaveDialog.Execute then begin
    CurrentPicture := SaveDialog.FileName;
  end;
  SavePicture(CurrentPicture);
end;
procedure TDForm.RedoClick(Sender: TObject);
begin
 if UndoFlag and not (Now = Length(ArrayOfActions)) then
 begin
     Now := Now + 1;
     CurrentFigures := RefreshArrays(ArrayOfActions[Now]);
   end;
 PaintBox.Invalidate;
end;

procedure TDForm.UndoClick(Sender: TObject);
begin
  if Length(ArrayOfActions) <> 0 then begin
  Now := Now - 1;
  RefreshFigures(Now);
  UndoFlag := True;
end;
  PaintBox.Invalidate;
end;
procedure TDForm.PasteSelectedClick(Sender: TObject);
var
  i, q: Integer;
  a: StringArray;
begin
if (Length(Buffer) <> 0) and BufferFlag then begin
    SetLength(CurrentFigures, Length(CurrentFigures) + Length(Buffer));
    for i := 0 to high(Buffer) do begin
    case Buffer[i].ClassName of

      'TPolyLine':         CurrentFigures[Length(CurrentFigures) + i] := TPolyLine.Create;
      'TLine'    :         CurrentFigures[Length(CurrentFigures) + i] := TLine.Create;
      'TEllipce' :         begin
                             CurrentFigures[Length(CurrentFigures) + i] := TEllipce.Create;
                             (CurrentFigures[Length(CurrentFigures) + i] as TObjectFigure).BrushColor := (Buffer[i] as TObjectFigure).BrushColor;
                             (CurrentFigures[Length(CurrentFigures) + i] as TObjectFigure).BrushStyle := (Buffer[i] as TObjectFigure).BrushStyle;
                           end;
      'TRectangle':        begin
                             CurrentFigures[Length(CurrentFigures) + i] := TRectangle.Create;
                             (CurrentFigures[Length(CurrentFigures) + i] as TObjectFigure).BrushColor := (Buffer[i] as TObjectFigure).BrushColor;
                             (CurrentFigures[Length(CurrentFigures) + i] as TObjectFigure).BrushStyle := (Buffer[i] as TObjectFigure).BrushStyle;
                           end;
      'TRoundedRectangle': begin
                             CurrentFigures[Length(CurrentFigures) + i] := TRoundedRectangle.Create;
                             (CurrentFigures[Length(CurrentFigures) + i] as TObjectFigure).BrushColor := (Buffer[i] as TObjectFigure).BrushColor;
                             (CurrentFigures[Length(CurrentFigures) + i] as TObjectFigure).BrushStyle := (Buffer[i] as TObjectFigure).BrushStyle;
                             (CurrentFigures[Length(CurrentFigures) + i] as TRoundedRectangle).RoundingRadiusX := (Buffer[i] as TRoundedRectangle).RoundingRadiusX;
                             (CurrentFigures[Length(CurrentFigures) + i] as TRoundedRectangle).RoundingRadiusY := (Buffer[i] as TRoundedRectangle).RoundingRadiusY;
                           end;
    end;

    for q := 0 to Length(Buffer[i].Points) do begin
      SetLength(CurrentFigures[Length(CurrentFigures) + i].Points, Length(CurrentFigures[Length(CurrentFigures) + i].Points) + 1);
      CurrentFigures[Length(CurrentFigures) + i].Points[q] := Buffer[i].Points[q];
    end;

    (CurrentFigures[Length(CurrentFigures) + i] as TLineFigure).PenColor := (Buffer[i] as TLineFigure).PenColor;
    (CurrentFigures[Length(CurrentFigures) + i] as TLineFigure).PenStyle := (Buffer[i] as TLineFigure).PenStyle;
    (CurrentFigures[Length(CurrentFigures) + i] as TLineFigure).Width := (Buffer[i] as TLineFigure).Width;
  end;
  end;
  Invalidate;
end;

procedure TDForm.CopySelectedClick(Sender: TObject);
var
  i, q: Integer;
  a: StringArray;
begin
  SetLength(Buffer, 0);
  for i := 0 to High(CurrentFigures) do begin
  BufferFlag := True;
  if CurrentFigures[i].Selected then begin

    SetLength(Buffer, Length(Buffer) + 1);
    case CurrentFigures[i].ClassName of

      'TPolyLine':         Buffer[High(Buffer)] := TPolyLine.Create;
      'TLine'    :         Buffer[High(Buffer)] := TLine.Create;
      'TEllipce' :         begin
                             Buffer[High(Buffer)] := TEllipce.Create;
                             (Buffer[High(Buffer)] as TObjectFigure).BrushColor := (CurrentFigures[i] as TObjectFigure).BrushColor;
                             (Buffer[High(Buffer)] as TObjectFigure).BrushStyle := (CurrentFigures[i] as TObjectFigure).BrushStyle;
                           end;
      'TRectangle':        begin
                             Buffer[High(Buffer)] := TRectangle.Create;
                             (Buffer[High(Buffer)] as TObjectFigure).BrushColor := (CurrentFigures[i] as TObjectFigure).BrushColor;
                             (Buffer[High(Buffer)] as TObjectFigure).BrushStyle := (CurrentFigures[i] as TObjectFigure).BrushStyle;
                           end;
      'TRoundedRectangle': begin
                             Buffer[High(Buffer)] := TRoundedRectangle.Create;
                             (Buffer[High(Buffer)] as TObjectFigure).BrushColor := (CurrentFigures[i] as TObjectFigure).BrushColor;
                             (Buffer[High(Buffer)] as TObjectFigure).BrushStyle := (CurrentFigures[i] as TObjectFigure).BrushStyle;
                             (Buffer[High(Buffer)] as TRoundedRectangle).RoundingRadiusX := (CurrentFigures[i] as TRoundedRectangle).RoundingRadiusX;
                             (Buffer[High(Buffer)] as TRoundedRectangle).RoundingRadiusY := (CurrentFigures[i] as TRoundedRectangle).RoundingRadiusY;
                           end;
    end;

    for q := 0 to Length(CurrentFigures[i].Points) do begin
      SetLength(Buffer[High(Buffer)].Points, Length(Buffer[High(Buffer)].Points) + 1);
      Buffer[High(Buffer)].Points[q] := CurrentFigures[i].Points[q];
    end;

    (Buffer[High(Buffer)] as TLineFigure).PenColor := (CurrentFigures[i] as TLineFigure).PenColor;
    (Buffer[High(Buffer)] as TLineFigure).PenStyle := (CurrentFigures[i] as TLineFigure).PenStyle;
    (Buffer[High(Buffer)] as TLineFigure).Width := (CurrentFigures[i] as TLineFigure).Width;
  end;
end;
end;
end.
