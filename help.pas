unit Help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { THelpForm }

  THelpForm = class(TForm)
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
 HelpForm: THelpForm;

implementation

{$R *.lfm}

{ THelpForm }

end.

