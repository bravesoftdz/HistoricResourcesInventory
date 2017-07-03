program HistoricResourcesAtlasGenerator;

uses
  FastMM4 in '..\lib\FastMM4.pas',
  FMX.Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  clipper in '..\lib\clipper.pas',
  Utils in '..\lib\Utils.pas',
  ArcViewFileUnit in '..\lib\ArcViewFileUnit.pas',
  HistoricalImpactZonesUnit in 'HistoricalImpactZonesUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
