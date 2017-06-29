program atlas_layout_editor;

uses
  FMX.Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form20},
  ArcViewFileUnit in '..\lib\ArcViewFileUnit.pas',
  clipper in '..\lib\clipper.pas',
  Utils in '..\lib\Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm20, Form20);
  Application.Run;
end.
