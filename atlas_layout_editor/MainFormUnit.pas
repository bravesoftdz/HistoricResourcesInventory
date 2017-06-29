unit MainFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;

type
  TForm20 = class(TForm)
    FDConnection: TFDConnection;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    Button1: TButton;
    AtlasLayoutsTable: TFDTable;
    AtlasBitmapsTable: TFDTable;
    AtlasLayoutsTableLayoutId: TFDAutoIncField;
    AtlasLayoutsTableDescription: TFDWideMemoField;
    AtlasLayoutsTableDPI: TIntegerField;
    AtlasBitmapsTableLayoutId: TIntegerField;
    AtlasBitmapsTableBmpNo: TIntegerField;
    AtlasBitmapsTablePageNo: TIntegerField;
    AtlasBitmapsTablePagePosX: TIntegerField;
    AtlasBitmapsTablePagePosY: TIntegerField;
    AtlasBitmapsTableCaption: TFDWideMemoField;
    AtlasBitmapsTableDimensionKind: TFDWideMemoField;
    AtlasBitmapsTableDimension: TFloatField;
    AtlasBitmapsTableDetail: TIntegerField;
    Query: TFDQuery;
    AtlasBitmapsTableLL_GISx: TFloatField;
    AtlasBitmapsTableLL_GISy: TFloatField;
    AtlasBitmapsTableUR_GISx: TFloatField;
    AtlasBitmapsTableUR_GISy: TFloatField;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form20: TForm20;

implementation

{$R *.fmx}

uses
   ArcViewFileUnit, Math, Utils;

procedure TForm20.Button1Click(Sender: TObject);
   const
      horizontal_pages = 2;
      page_image_width_in_inches = 14.5;
      page_image_height_in_inches = 8;
      layout_id = 1;
   var
      bmp_no, x, y, vertical_pages: integer;
      muni_border_shape_file: TArcViewShapeFile;
      muni_border: TArcViewPolygonShape;
      horizontal_gis_units_per_page, vertical_gis_units_per_page: real;
      vertical_offset_in_gis_units: real;
   begin
      with FDConnection do
         begin
            Close;
            with Params do
               begin
                  Clear;
                  Add('DriverID=SQLite');
                  Add('Database=' + ConvertRelToAbsPath('..\db', ExtractFilePath(ParamStr(0))) + '\HistoricInventory.db')
               end;
            Open
         end;

      Query.SQL.Clear;
      Query.SQL.Add ('delete');
      Query.SQL.Add ('from AtlasBitmaps');
      Query.SQL.Add ('where Detail=0 and LayoutId=' + IntToStr(layout_id));
      Query.ExecSQL;

      muni_border_shape_file := TArcViewShapeFile.CreateFromFile ('..\db\muni_border.shp');
      assert (muni_border_shape_file.number_of_shapes = 1);
      muni_border := TArcViewPolygonShape(muni_border_shape_file[1]);
      with muni_border do
         begin
            vertical_pages := Ceil (horizontal_pages
                                    *
                                    ((bounding_box_Ymax - bounding_box_Ymin) / (bounding_box_Xmax - bounding_box_Xmin))
                                     /
                                    (page_image_height_in_inches / page_image_width_in_inches)
                                   );
            horizontal_gis_units_per_page := (bounding_box_Xmax - bounding_box_Xmin) / horizontal_pages;
            vertical_gis_units_per_page := horizontal_gis_units_per_page * page_image_height_in_inches / page_image_width_in_inches;
            vertical_offset_in_gis_units := ((vertical_pages * vertical_gis_units_per_page) - (bounding_box_Ymax - bounding_box_Ymin)) / 2
         end;

      AtlasBitmapsTable.Active := true;
      bmp_no := 1;
      for y := 0 to vertical_pages-1 do
         for x := 0 to horizontal_pages-1 do
            begin
               bmp_no := bmp_no + 1;
               with muni_border do
                  begin
                     AtlasBitmapsTable.Append;
                     AtlasBitmapsTableLayoutId.AsInteger := layout_id;
                     AtlasBitmapsTableBmpNo.AsInteger := bmp_no;
                     AtlasBitmapsTablePageNo.AsInteger := bmp_no;
                     AtlasBitmapsTablePagePosX.AsInteger := 1;
                     AtlasBitmapsTablePagePosY.AsInteger := 1;
                     AtlasBitmapsTableLL_GISx.AsFloat := bounding_box_Xmin + (x * horizontal_gis_units_per_page);
                     AtlasBitmapsTableLL_GISy.AsFloat := bounding_box_Ymax - ((y+1) * vertical_gis_units_per_page) + vertical_offset_in_gis_units;
                     AtlasBitmapsTableUR_GISx.AsFloat := bounding_box_Xmin + ((x+1) * horizontal_gis_units_per_page);
                     AtlasBitmapsTableUR_GISy.AsFloat := bounding_box_Ymax - (y * vertical_gis_units_per_page) + vertical_offset_in_gis_units;
                     AtlasBitmapsTableDimensionKind.AsString := 'W';
                     AtlasBitmapsTableDimension.AsFloat := page_image_width_in_inches;
                     AtlasBitmapsTableDetail.AsInteger := 0;
                     AtlasBitmapsTable.Post
                  end;
            end;
      AtlasBitmapsTable.Active := false
   end;

end.
