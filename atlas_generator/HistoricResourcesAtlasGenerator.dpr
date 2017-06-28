program HistoricResourcesAtlasGenerator;

uses
  FastMM4 in '..\lib\FastMM4.pas',
  FMX.Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form18},
  clipper in '..\lib\clipper.pas',
  Utils in '..\lib\Utils.pas',
  ArcViewFileUnit in '..\lib\ArcViewFileUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm18, Form18);
  Application.Run;
end.
