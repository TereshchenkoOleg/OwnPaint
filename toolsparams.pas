unit toolsparams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, Dialogs, Spin,
  StdCtrls;
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
  const BStyles: array [0..7] of TBrushStyle = (bsClear, bsSolid,
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
  var
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
  StyleLabel.Caption := 'Стиль заливки ';
  StyleLabel.Top := 160;
  StyleLabel.Parent:=Panel;

  BrushStyle := TComboBox.Create(Panel);
  for i:=0 to 7 do
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
end.

