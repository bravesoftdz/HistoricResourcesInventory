UNIT MainFormUnit;

INTERFACE

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.frxClass, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLite,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.FMXUI.Wait,
  FireDAC.Comp.UI, FMX.Layouts, FMX.Memo, FMX.ExtCtrls, ArcViewFileUnit;

const
   // color combinations courtesy of colorbrewer2.org, Color Advice For Cartography
   //   http://colorbrewer2.org/#type=sequential&scheme=OrRd&n=4
   //   this color combination is �colorblind safe�, �printer friendly� and �photocopy safe�.
   class_I_color      = TAlphaColor($FFfd301f);
   class_II_color     = TAlphaColor($FFfc8d59);
   class_III_color    = TAlphaColor($FFfdcc8a);
   unclassified_color = TAlphaColor($FFfef0d9);

   intact_color = TAlphaColors.Green;
   nonintact_color = TAlphaColors.Yellow;
   archaeological_site_color = TAlphaColors.Darkslategray;

   // constants for drawing resource id circles
   status_circle_radius_multiplier = 0.4875;
   ring_circle_radius_multiplier = 1.2;
   marker_radius_multiplier = 1.3;

   tie_width_dpi_divisor = 10;
   tie_spacing_dpi_divisor = 5;

   tic_mark_interval = 1000;   // in GIS units (ft)

   border_stroke_thickness = 10;
   muni_border_stroke_thickness = 2;
   parcel_border_stroke_thickness = 1;

type
   t_resource_classification =
      (c_50plus_archaeological,
       c_50plus_nonintact,
       c_50plus_intact,
       c_local_archaeological,
       c_local_nonintact,
       c_local_intact,
       c_national_archaeological,
       c_national_nonintact,
       c_national_intact
      );
   t_resource_classification_set = set of t_resource_classification;

type
   tResource =
      record
         id: integer;
//            pin: string;
         gisx, gisy: real;
         non_point_resource: boolean;
//            photo_page: integer;
//            photo_fn: string;
         addr911: string;
         tax_parcel_idx: integer;
         ring_color: TAlphaColor;
         status_color: TAlphaColor;
         classification: t_resource_classification;
         map_pages: string;
         map_grid_location: string;
         function set_classification (s: string): boolean;
      end;

type
   tMapLayout =
      record
         border_stroke_thickness: real;
      end;

type
  TMainForm = class(TForm)
    FDConnection: TFDConnection;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    NGParcelTable: TFDTable;
    ResourcesTable: TFDTable;
    NGParcelTablePIN_COMMON: TFDWideMemoField;
    ResourcesTableResourceId: TFDAutoIncField;
    ResourcesTableClassification: TFDWideMemoField;
    ResourcesTableStreetNo: TFDWideMemoField;
    ResourcesTableStreetName: TFDWideMemoField;
    ResourcesTableGISx: TFloatField;
    ResourcesTableGISy: TFloatField;
    ResourcesTableNonPointResource: TIntegerField;
    Memo1: TMemo;
    AtlasBitmapsTable: TFDTable;
    AtlasBitmapsTableLayoutId: TIntegerField;
    AtlasBitmapsTablePageNo: TIntegerField;
    AtlasBitmapsTablePagePosX: TIntegerField;
    AtlasBitmapsTablePagePosY: TIntegerField;
    AtlasBitmapsTableCaption: TFDWideMemoField;
    AtlasBitmapsTableDimensionKind: TFDWideMemoField;
    AtlasBitmapsTableDimension: TFloatField;
    Button1: TButton;
    AtlasBitmapsTableMapId: TFDWideMemoField;
    AtlasBitmapsTableDetail: TIntegerField;
    AtlasBitmapsTableLL_GISx: TFloatField;
    AtlasBitmapsTableLL_GISy: TFloatField;
    AtlasBitmapsTableUR_GISx: TFloatField;
    AtlasBitmapsTableUR_GISy: TFloatField;
    AtlasLayoutsTable: TFDTable;
    AtlasLayoutsTableLayoutId: TFDAutoIncField;
    AtlasLayoutsTableDescription: TFDWideMemoField;
    AtlasLayoutsTableDPI: TIntegerField;
    AtlasLayoutsTableResourceFontSize: TIntegerField;
    AtlasLayoutsTableDetailCutoutFontSize: TIntegerField;
    AtlasLayoutsTableBorder: TFloatField;
    AtlasLayoutsTableBorderFontSize: TIntegerField;
    ResourceMapGridLocationsTable: TFDTable;
    ResourceMapGridLocationsTableResourceId: TIntegerField;
    ResourceMapGridLocationsTableMapGridLocation: TFDWideMemoField;
    Query: TFDQuery;
    Button2: TButton;
    GroupBox1: TGroupBox;
    CountyStyleRadioButton: TRadioButton;
    ImpactZonesRadioButton: TRadioButton;
    NoColoringRadioButton: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    type
      t_background_coloring_style = (bcs_no_coloring, bcs_county_style, bcs_impact_zones);
    var
      parcels: TArcViewShapeFile;
      national_resources_impact_zones: TArcViewShapeFile;
      local_resources_impact_zones: TArcViewShapeFile;
      fifty_plus_resources_impact_zones: TArcViewShapeFile;
      muni_border: TArcViewPolygonShape;
      grid_origin: TGISPoint;  { upper left corner }
      parcel_data:
         array {1..N} of     // 0th element allocated but not used
            record
               pin_common: string;
               color: TAlphaColor
            end;
      bitmaps:
         array of
            record
               caption: string;
               map_id: string;
               page_no: integer;
               detail: boolean;
               ll_GISx: real;
               ll_GISy: real;
               ur_GISx: real;
               ur_GISy: real;
               bmp_width_in_pixels: integer;
               bmp_height_in_pixels: integer
            end;
      resources: array of tResource;
      dpi: integer;
      border_width: real;  // in inches
      border_font_size: integer;
      resource_circle_font_size: integer;
      detail_font_size: integer;


      current_page_border: TArcViewPolygonShape;
    procedure set_background_coloring_style (s: t_background_coloring_style);
    function get_background_coloring_style:  t_background_coloring_style;
    function map_x_grid_string (x: real {GIS units}): string;
    function map_y_grid_string (y: real {GIS units}): string;
    procedure draw_map_bitmap (map_id: string; bmp_image_width_in_pixels, bmp_image_height_in_pixels: integer; detail_map: boolean);
    property background_coloring_style: t_background_coloring_style read get_background_coloring_style write set_background_coloring_style;
  public
    { Public declarations }
  end;

var
   MainForm: TMainForm;

IMPLEMENTATION

{$R *.fmx}

uses
   Utils, FMX.Printer, Winapi.Windows, System.UIConsts, fmx.objects,
   System.Math, HistoricalImpactZonesUnit;


function tResource.set_classification (s: string): boolean;
   begin
      result := true;  // provisional
      if s = 'I' then
         begin
            classification := c_national_intact;
            ring_color := class_I_color;
            status_color := intact_color
         end
      else if s = 'I(N)' then
         begin
            classification := c_national_nonintact;
            ring_color := class_I_color;
            status_color := nonintact_color
         end
      else if s = 'I(A)' then
         begin
            classification := c_national_archaeological;
            ring_color := class_I_color;
            status_color := archaeological_site_color
         end
      else if s = 'II' then
         begin
            classification := c_local_intact;
            ring_color := class_II_color;
            status_color := intact_color
         end
      else if s = 'II(N)' then
         begin
            classification := c_local_nonintact;
            ring_color := class_II_color;
            status_color := nonintact_color
         end
      else if s = 'II(A)' then
         begin
            classification := c_local_archaeological;
            ring_color := class_II_color;
            status_color := archaeological_site_color
         end
      else if s = 'III' then
         begin
            classification := c_50plus_intact;
            ring_color := class_III_color;
            status_color := intact_color
         end
      else if s = 'III(N)' then
         begin
            classification := c_50plus_nonintact;
            ring_color := class_III_color;
            status_color := nonintact_color
         end
      else if s = 'III(A)' then
         begin
            classification := c_50plus_archaeological;
            ring_color := class_III_color;
            status_color := archaeological_site_color
         end
      else   // unknown classification
         begin
            result := false;
            classification := c_50plus_archaeological;
            ring_color := TColors.White;
            status_color := TColors.White
         end
   end;

function TMainForm.map_x_grid_string (x: real {GISunits}): string;
   var
      i: integer;
   begin
      i := round (x - grid_origin.x) div tic_mark_interval;
      if i < 26 then
         result := chr(ord('A')+i)
      else
         begin
            result := '..';
            result [1] := chr(ord('A')+(i div 26)-1);
            result [2] := chr(ord('A')+(i mod 26))
         end
   end;

function TMainForm.map_y_grid_string (y: real {GIS units}): string;
   begin
      result := IntToStr (1 + Floor ((grid_origin.y - y) / tic_mark_interval))
   end;

procedure TMainForm.Button2Click(Sender: TObject);
var h: tHistoricalImpactZones;
begin
//    h := tHistoricalImpactZones.Create;
//    h.AddPointResource (GISPoint(0,0), 100);
//    h.AddPointResource (GISPoint(50,0), 100);
//    h.Free
end;

procedure TMainForm.draw_map_bitmap (map_id: string; bmp_image_width_in_pixels, bmp_image_height_in_pixels: integer; detail_map: boolean);
   var
      bmp: FMX.Graphics.TBitmap;
      pixels_per_gis_unit: real;
      marker_radius: real;
      ring_circle_radius: real;
      status_circle_radius: real;

   function pixelX (gisX: real): integer;   // convert gisX to pixelX on current page
      begin
         pixelX := round ((border_width*dpi) + (gisX - current_page_border.Part[0,0].x) * pixels_per_gis_unit)
      end;

   function pixelY (gisY: real): integer;   // convert gisY to pixelY on current page
      begin
         result := round ((border_width*dpi) + (current_page_border.Part[0,0].y - gisY) * pixels_per_gis_unit)
      end;

   function RectFforCenteredText (center: TPointF {pixels}; s: string): TRectF;
      begin
         result.Top := center.y - bmp.Canvas.TextHeight(s);
         result.Bottom := center.y + bmp.Canvas.TextHeight(s);
         result.Left := center.x - bmp.Canvas.TextWidth(s);
         result.Right := center.x + bmp.Canvas.TextWidth(s)
      end;

   procedure draw_parcels;
      var
         parcel_no: integer;

      procedure fill_portion_of_parcel_in_impact_zone (zone: TArcViewShapeFile; color: TAlphaColor);
         var
            portion_of_parcel_in_impact_zone: TArcViewPolygonShape;
            z,p,i: integer;
            points: TPolygon;  // array of TPoint;
         begin
            for z := 1 to zone.number_of_shapes do
               begin
                  portion_of_parcel_in_impact_zone := TArcViewPolygonShape(parcels.shapes[parcel_no]).Intersection(TArcViewPolygonShape(zone.shapes[z]));
                  with portion_of_parcel_in_impact_zone do
                     for p := 0 to Length(Part)-1 do
                        begin
                           SetLength (points, Length(Part[p])+1);
                           for i := 0 to Length(Part[p])-1
                           do begin
                                 points[i].x := pixelX(Part[p][i].x);
                                 points[i].y := pixelY(Part[p][i].y)
                              end;
                           points[Length(Part[p])].x := pixelX(Part[p][0].x);
                           points[Length(Part[p])].y := pixelY(Part[p][0].y);

                           bmp.Canvas.Fill.Color := color;
                           bmp.Canvas.FillPolygon (points, 1)
                        end;
                  portion_of_parcel_in_impact_zone.Free
               end
         end;

      var
         p,i: integer;
         portion_of_parcel_within_muni_boundary: TArcViewPolygonShape;
         points: TPolygon;  // array of TPoint;
      begin
         bmp.Canvas.StrokeThickness := parcel_border_stroke_thickness;
         for parcel_no := 1 to parcels.number_of_shapes do
            begin
               portion_of_parcel_within_muni_boundary := TArcViewPolygonShape(parcels.shapes [parcel_no]).Intersection(muni_border);
               with portion_of_parcel_within_muni_boundary do
                  for p := 0 to Length(Part)-1 do
                     begin
                        SetLength (points, Length(Part[p])+1);
                        for i := 0 to Length(Part[p])-1
                        do begin
                              points[i].x := pixelX(Part[p][i].x);
                              points[i].y := pixelY(Part[p][i].y)
                           end;
                        points[Length(Part[p])].x := pixelX(Part[p][0].x);
                        points[Length(Part[p])].y := pixelY(Part[p][0].y);

                        case background_coloring_style of
                           bcs_no_coloring:
                              begin
                                 bmp.Canvas.Fill.Color := unclassified_color;
                                 bmp.Canvas.FillPolygon (points, 1);
                              end;
                           bcs_county_style:
                              begin
                                 bmp.Canvas.Fill.Color := parcel_data[parcel_no].color;
                                 bmp.Canvas.FillPolygon (points, 1);
                              end;
                           bcs_impact_zones:
                              begin
                                 bmp.Canvas.Fill.Color := unclassified_color;
                                 bmp.Canvas.FillPolygon (points, 1);

                                 fill_portion_of_parcel_in_impact_zone (fifty_plus_resources_impact_zones , class_III_color);
                                 fill_portion_of_parcel_in_impact_zone (local_resources_impact_zones , class_II_color);
                                 fill_portion_of_parcel_in_impact_zone (national_resources_impact_zones , class_I_color)
                              end
                        else
                           assert (false)
                        end;

                        bmp.Canvas.DrawPolygon (points, 1)
                     end;
               portion_of_parcel_within_muni_boundary.Free
            end
      end;

   procedure draw_impact_zones (z: TArcViewShapeFile; fill_color: TAlphaColor);
      var
         sh, i, p: integer;
         points: TPolygon;  //array of TPoint;
      begin
         for sh := 1 to z.number_of_shapes do
            with TArcViewPolygonShape(z.shapes[sh]) do
               for p := 0 to Length(Part)-1 do
                  begin
                     SetLength (points, Length(Part[p])+1);
                     for i := 0 to Length(Part[p])-1
                     do begin
                           points[i].x := pixelX(Part[p][i].x);
                           points[i].y := pixelY(Part[p][i].y)
                        end;
                     points[Length(Part[p])].x := pixelX(Part[p][0].x);
                     points[Length(Part[p])].y := pixelY(Part[p][0].y);
                     bmp.Canvas.Fill.Color := fill_color;
                     bmp.Canvas.FillPolygon (points, 1)
                  end
      end;

   procedure draw_blank_muni;
      var
         i: integer;
         points: TPolygon;  //array of TPoint;
      begin
         with muni_border do
            begin
               assert (Length(Part) = 1);
               SetLength (points, Length(Part[0])+1);
               for i := 0 to Length(Part[0])-1
               do begin
                     points[i].x := pixelX(Part[0][i].x);
                     points[i].y := pixelY(Part[0][i].y)
                  end;
               points[Length(Part[0])].x := pixelX(Part[0][0].x);
               points[Length(Part[0])].y := pixelY(Part[0][0].y);

               bmp.Canvas.Fill.Color := TAlphaColors.White;
               bmp.Canvas.FillPolygon (points, 1);

               bmp.Canvas.Stroke.Cap := TStrokeCap.scRound;
               bmp.Canvas.Stroke.Color := TAlphaColors.Black;
               bmp.Canvas.StrokeThickness := muni_border_stroke_thickness;
               bmp.Canvas.DrawPolygon (points, 1)
            end
      end;

   procedure draw_streams;
      function inside_muni_border (x, y: real): boolean;
         var
            pt: TGISPoint;
         begin
            pt.x := x;
            pt.y := y;
            result := muni_border.contains(pt)
         end;
      var
         streams: TArcViewShapeFile;
         p,i: integer;
         sh: integer;
         p1, p2: TPointF;
         old_stroke_thickness: single;
         old_stroke_color: TAlphaColor;
      begin
         streams := TArcViewShapeFile.CreateFromFile ('..\db\Rivers_and_Streams.shp');
         old_stroke_thickness := bmp.Canvas.StrokeThickness;
         old_stroke_color := bmp.Canvas.Stroke.Color;
         bmp.Canvas.Stroke.Cap := TStrokeCap.scRound;
         bmp.Canvas.Stroke.Color := TAlphaColors.Blue;
         bmp.Canvas.StrokeThickness := bmp.Canvas.StrokeThickness * 10;
         for sh := 1 to streams.number_of_shapes do
            if streams[sh] is TArcViewPolyLineShape then
               with streams[sh] as TArcViewPolyLineShape do
                  if bounding_box.Intersects (current_page_border) then
                     for p := 0 to Length(Part)-1 do
                        for i := 0 to Length(Part[p])-2 do
                           begin
                              p1.x := pixelX(Part[p][i].x);
                              p1.y := pixelY(Part[p][i].y);
                              p2.x := pixelX(Part[p][i+1].x);
                              p2.y := pixelY(Part[p][i+1].y);
                              bmp.Canvas.DrawLine(p1, p2, 1)
                           end;
         streams.Free;
         bmp.Canvas.StrokeThickness := old_stroke_thickness;
         bmp.Canvas.Stroke.Color := old_stroke_color
      end;

   procedure draw_railroad (fn: string);
      var
         tie_width_in_pixels: integer;
         tie_spacing_in_pixels: integer;

      function segment_length (p1, p2: TGISPoint): real;
         begin
            result := sqrt (sqr(p2.x - p1.x) + sqr(p2.Y - p1.y))
         end;

      procedure draw_tie (p: TGISPoint; angle: real {in radians});
         var
            right_angle: real;
            p1,p2: TPointF;
         begin
            right_angle := angle + (pi/2);
            p1.X := pixelX (p.x + (cos(right_angle)*tie_width_in_pixels));
            p1.Y := pixelY (p.y + (sin(right_angle)*tie_width_in_pixels));
            p2.X := pixelX (p.x - (cos(right_angle)*tie_width_in_pixels));
            p2.Y := pixelY (p.y - (sin(right_angle)*tie_width_in_pixels));
            bmp.Canvas.DrawLine(p1, p2, 1)
         end;
      var
         rr: TArcViewShapeFile;
         p,i: integer;
         rr_no: integer;
         old_stroke_thickness: single;
         old_stroke_color: TAlphaColor;
         tie_spacing_in_gis_units: real;
         angle: real;
         hypotenuse: real;
         dist: real;
         px1, px2: TPointF;
         pg1, pg2: TGISPoint;
      begin
         tie_width_in_pixels := dpi div tie_width_dpi_divisor;
         tie_spacing_in_pixels := dpi div tie_spacing_dpi_divisor;

         tie_spacing_in_gis_units := tie_spacing_in_pixels / pixels_per_gis_unit;
         old_stroke_thickness := bmp.Canvas.StrokeThickness;
         old_stroke_color := bmp.Canvas.Stroke.Color;
         bmp.Canvas.Stroke.Cap := TStrokeCap.scRound;
         bmp.Canvas.Stroke.Color := TAlphaColors.Black;
         bmp.Canvas.StrokeThickness := bmp.Canvas.StrokeThickness * 10;
         rr := TArcViewShapeFile.CreateFromFile ('..\db\' + fn + '.shp');
         for rr_no := 1 to rr.number_of_shapes do
            if rr[rr_no] is TArcViewPolyLineShape then
               with rr[rr_no] as TArcViewPolyLineShape do
                  if bounding_box.Intersects (current_page_border) then
                     begin
                        // draw main line
                        for p := 0 to Length(Part)-1 do
                           for i := 0 to Length(Part[p])-2 do
                              begin
                                 px1.x := pixelX(Part[p][i].x);
                                 px1.y := pixelY(Part[p][i].y);
                                 px2.x := pixelX(Part[p][i+1].x);
                                 px2.y := pixelY(Part[p][i+1].y);
                                 bmp.Canvas.DrawLine(px1, px2, 1)
                              end;
                        // draw ties
                        for p := 0 to Length(Part)-1 do
                           begin
                              dist := tie_spacing_in_gis_units / 2;
                              for i := 0 to Length(Part[p])-2 do
                                 begin
                                    pg1 := Part[p][i];
                                    pg2 := Part[p][i+1];
                                    hypotenuse := segment_length (pg1, pg2);
                                    if hypotenuse > 0 then
                                       begin
                                          if pg2.X >= pg1.X then
                                             angle := arcsin ((pg2.Y-pg1.Y) / hypotenuse)
                                          else
                                             angle := pi - arcsin ((pg2.Y-pg1.Y) / hypotenuse);
                                          pg2.x := pg1.x + round(cos(angle) * dist);
                                          pg2.y := pg1.y + round(sin(angle) * dist);
                                          while dist <= hypotenuse do
                                             begin
                                                draw_tie (pg2, angle);
                                                pg1 := pg2;
                                                pg2.x := pg1.x + round(cos(angle) * tie_spacing_in_gis_units);
                                                pg2.y := pg1.y + round(sin(angle) * tie_spacing_in_gis_units);
                                                dist := dist + tie_spacing_in_gis_units
                                             end;
                                          dist := dist - hypotenuse
                                       end
                                 end
                           end
                     end;
         rr.Free;
         bmp.Canvas.StrokeThickness := old_stroke_thickness;
         bmp.Canvas.Stroke.Color := old_stroke_color
      end;

   procedure draw_resource_marker (id: integer; gisx, gisy: real);
      var
         s: string;
         rect: TRectF;
         fudge_down: real;
         fudge_right: real;
         pt1, pt2: TPointF;
         old_stroke_thickness: single;
      begin   // draw_resource_marker
         // draw black outer ring
         rect.Left := pixelX(gisx)-marker_radius;
         rect.Top := pixelY(gisy)-marker_radius;
         rect.Right := pixelX(gisx)+marker_radius;
         rect.Bottom := pixelY(gisy)+marker_radius;
         bmp.Canvas.Fill.Color := TAlphaColors.Black;
         bmp.Canvas.FillEllipse (rect, 1);

         // draw classification ring inside outer ring
         rect.Left := pixelX(gisx)-ring_circle_radius;
         rect.Top := pixelY(gisy)-ring_circle_radius;
         rect.Right := pixelX(gisx)+ring_circle_radius;
         rect.Bottom := pixelY(gisy)+ring_circle_radius;
         bmp.Canvas.Fill.Color := resources[id].ring_color;
         bmp.Canvas.FillEllipse (rect, 1);

         // draw status circle inside classification ring
         rect.Left := pixelX(gisx)-status_circle_radius;
         rect.Top := pixelY(gisy)-status_circle_radius;
         rect.Right := pixelX(gisx)+status_circle_radius;
         rect.Bottom := pixelY(gisy)+status_circle_radius;
         bmp.Canvas.Fill.Color := resources[id].status_color;
         bmp.Canvas.FillEllipse (rect, 1);
         bmp.canvas.Stroke.Kind := TBrushKind.bkSolid;
         bmp.canvas.StrokeThickness := 1;

         if resources[id].status_color = archaeological_site_color then
            begin   // draw X to denote gone
               old_stroke_thickness := bmp.Canvas.StrokeThickness;
               bmp.Canvas.StrokeThickness := bmp.Canvas.StrokeThickness * 3;

               pt1.X := pixelX(gisx)-marker_radius;
               pt1.Y := pixelY(gisy)-marker_radius;
               pt2.X := pixelX(gisx)+marker_radius;
               pt2.Y := pixelY(gisy)+marker_radius;
               bmp.Canvas.DrawLine (pt1, pt2, 1);

               pt1.X := pixelX(gisx)+marker_radius;
               pt1.Y := pixelY(gisy)-marker_radius;
               pt2.X := pixelX(gisx)-marker_radius;
               pt2.Y := pixelY(gisy)+marker_radius;
               bmp.Canvas.DrawLine (pt1, pt2, 1);

               bmp.Canvas.StrokeThickness := old_stroke_thickness
            end;

         // write resource id number
         s := IntToStr(id);
         case resources[id].status_color of
            intact_color:
               bmp.Canvas.Fill.Color := TAlphaColors.White;
            nonintact_color:
               bmp.Canvas.Fill.Color := TAlphaColors.Black;
            archaeological_site_color:
               bmp.Canvas.Fill.Color := TAlphaColors.White;
         else
            assert(false)
         end;

         fudge_down := bmp.Canvas.TextHeight(s) * 0.065;
         fudge_right := bmp.Canvas.TextWidth(s) * 0.02;
         rect.Left := pixelX(gisx) - (bmp.Canvas.TextWidth(s) / 2) + fudge_right;
         rect.Top := pixelY(gisy) - (bmp.Canvas.TextHeight(s) / 2) + fudge_down;
         rect.Right := pixelX(gisx) + (bmp.Canvas.TextWidth(s) / 2) + fudge_right;
         rect.Bottom := pixelY(gisy) + (bmp.Canvas.TextHeight(s) / 2) + fudge_down;
         bmp.Canvas.FillText(rect, s, false, 1, [TFillTextFlag.ftRightToLeft], TTextAlign.taCenter, TTextAlign.taCenter);
      end;    // draw_resource_marker

   procedure draw_detail_boxes;
      var
         i: integer;
         rect: TRectF;
         x,y: real;
      function center (l,h: real): real;
         begin
            result := l + ((h-l)/2)
         end;
      procedure out (s: string);
         begin
            rect.Left := x - (bmp.Canvas.TextWidth(s) / 2);
            rect.Top := y - (bmp.Canvas.TextHeight(s) / 2);
            rect.Right := x + (bmp.Canvas.TextWidth(s) / 2);
            rect.Bottom := y + (bmp.Canvas.TextHeight(s) / 2);
            bmp.Canvas.FillText(rect, s, false, 1, [TFillTextFlag.ftRightToLeft], TTextAlign.taCenter, TTextAlign.taCenter);
            y := y + bmp.Canvas.TextHeight(s)
         end;
      begin
         for i := 0 to Length(bitmaps)-1 do
            with bitmaps[i] do
               if detail then
                  begin
                     bmp.Canvas.Fill.Color := TAlphaColors.White;
                     rect := TRectF.Create (pixelX(ll_GISx), pixelY(ur_GISy), pixelX(ur_GISx), pixelY(ll_GISy));
                     bmp.Canvas.FillRect (rect, 0, 0, AllCorners, 0.85);
                     bmp.Canvas.Fill.Color := TAlphaColors.Black;
                     x := center (pixelX (ll_GISx), pixelX (ur_GISx));
                     y := center (pixelY(ll_GISy), pixelY(ur_GISy)) - (bmp.Canvas.TextHeight('X') / 2);
                     out ('See p. ' + IntToStr(page_no));
                     out (caption)
                  end
      end;

   procedure draw_map_grid;
      var
         x, y: real;
      begin
         x := grid_origin.x;
         while x < current_page_border.bounding_box_Xmax do
            begin
               bmp.Canvas.DrawLine (PointF (PixelX(x), PixelY(current_page_border.bounding_box_Ymin)),
                                    PointF (PixelX(x), PixelY(current_page_border.bounding_box_Ymax)),
                                    1
                                   );
               x := x + tic_mark_interval
            end;

         y := grid_origin.y;
         while y > current_page_border.bounding_box_Ymin do
            begin
               bmp.Canvas.DrawLine (PointF (PixelX(current_page_border.bounding_box_Xmin), PixelY(y)),
                                    PointF (PixelX(current_page_border.bounding_box_Xmax), PixelY(y)),
                                    1
                                   );
               y := y - tic_mark_interval
            end
      end;

   procedure draw_border;
      var
         r: TRectF;
         border_width_in_pixels: real;
         x, y: real;
         s: string;
         i: integer;
         half_border_stroke_thickness: real;
      begin
         bmp.Canvas.StrokeThickness := border_stroke_thickness;
         bmp.Canvas.Stroke.Cap := TStrokeCap.scFlat;

         border_width_in_pixels := dpi * border_width;
         half_border_stroke_thickness := border_stroke_thickness / 2;

         // white out border area erasing non-clipped stuff that has been drawn there
         bmp.Canvas.Fill.Color := TAlphaColors.White;
         bmp.Canvas.FillRect (RectF (0, 0, bmp.Width, border_width_in_pixels), 0, 0, AllCorners, 1);
         bmp.Canvas.FillRect (RectF (0, 0, border_width_in_pixels, bmp.Height), 0, 0, AllCorners, 1);
         bmp.Canvas.FillRect (RectF (bmp.Width - border_width_in_pixels, 0, bmp.Width, bmp.Height), 0, 0, AllCorners, 1);
         bmp.Canvas.FillRect (RectF (0, bmp.Height - border_width_in_pixels, bmp.Width, bmp.Height), 0, 0, AllCorners, 1);

         bmp.Canvas.Fill.Color := TAlphaColors.Black;

         // draw box around map
         r := RectF (border_width_in_pixels-half_border_stroke_thickness,
                     border_width_in_pixels-half_border_stroke_thickness,
                     bmp.Width-border_width_in_pixels+half_border_stroke_thickness,
                     bmp.Height-border_width_in_pixels+half_border_stroke_thickness
                    );
         bmp.Canvas.DrawRect (r, 0, 0, AllCorners, 1);

         x := grid_origin.x;
         while x <= current_page_border.bounding_box_Xmax do
            begin
               if x >= current_page_border.bounding_box_Xmin then
                  begin
                     bmp.Canvas.DrawLine (PointF (PixelX(x), 0),
                                          PointF (PixelX(x), border_width_in_pixels),
                                          1
                                         );
                     bmp.Canvas.DrawLine (PointF (PixelX(x), bmp.Height-border_width_in_pixels),
                                          PointF (PixelX(x), bmp.Height),
                                          1
                                         )
                  end;
               x := x + tic_mark_interval
            end;

         x := grid_origin.x + (tic_mark_interval / 2);
         while x < current_page_border.bounding_box_Xmax do
            begin
               s := map_x_grid_string (x);
               r := RectFforCenteredText (PointF (PixelX(x), (border_width_in_pixels-border_stroke_thickness)/2), s);
               if (PixelX(current_page_border.bounding_box_Xmin) <= r.Left) and (r.Right <= PixelX(current_page_border.bounding_box_Xmax)) then
                  begin
                     bmp.Canvas.FillText (r, s, false, 1, [TFillTextFlag.ftRightToLeft], TTextAlign.taCenter, TTextAlign.taCenter);
                     r := RectFforCenteredText (PointF (PixelX(x), bmp.Height - ((border_width_in_pixels-border_stroke_thickness)/2)), s);
                     bmp.Canvas.FillText (r, s, false, 1, [TFillTextFlag.ftRightToLeft], TTextAlign.taCenter, TTextAlign.taCenter)
                  end;
               x := x + tic_mark_interval
            end;

         y := grid_origin.y;
         while y >= current_page_border.bounding_box_Ymin do
            begin
               if y <= current_page_border.bounding_box_Ymax then
                  begin
                     bmp.Canvas.DrawLine (PointF (0, PixelY(y)),
                                          PointF (border_width_in_pixels, PixelY(y)),
                                          1
                                         );
                     bmp.Canvas.DrawLine (PointF (bmp.Width-border_width_in_pixels, PixelY(y)),
                                          PointF (bmp.Width, PixelY(y)),
                                          1
                                         )
                  end;
               y := y - tic_mark_interval
            end;

         y := grid_origin.y - (tic_mark_interval / 2);
         while y >= current_page_border.bounding_box_Ymin do
            begin
               s := map_y_grid_string (y);
               r := RectFforCenteredText (PointF((border_width_in_pixels-border_stroke_thickness)/2, PixelY(y)), s);
               if (PixelY(current_page_border.bounding_box_Ymax) <= r.Top) and (r.Bottom <= PixelY(current_page_border.bounding_box_Ymin)) then
                  begin
                     bmp.Canvas.FillText (r, s, false, 1, [TFillTextFlag.ftRightToLeft], TTextAlign.taCenter, TTextAlign.taCenter);
                     r := RectFforCenteredText (PointF (bmp.Width - ((border_width_in_pixels-border_stroke_thickness)/2), PixelY(y)), s);
                     bmp.Canvas.FillText (r, s, false, 1, [TFillTextFlag.ftRightToLeft], TTextAlign.taCenter, TTextAlign.taCenter)
                  end;
               y := y - tic_mark_interval
            end
      end;

   var
      i: integer;
      c: t_resource_classification;
   begin    // draw_map_bitmap
      pixels_per_gis_unit := bmp_image_width_in_pixels / current_page_border.width;

      bmp := FMX.Graphics.TBitmap.Create(bmp_image_width_in_pixels + round(2*dpi*border_width), bmp_image_height_in_pixels + round(2*dpi*border_width));
      bmp.Canvas.BeginScene;
      bmp.Canvas.Clear(TAlphaColors.White);

      if background_coloring_style = bcs_impact_zones then
         begin
            // draw impact zones first - these will be the portion outside the muni boundaries
            draw_impact_zones (fifty_plus_resources_impact_zones, class_III_color);
            draw_impact_zones (local_resources_impact_zones, class_II_color);
            draw_impact_zones (national_resources_impact_zones, class_I_color)
         end;

      draw_blank_muni;

      draw_parcels;

      draw_map_grid;
      draw_streams;
      draw_railroad ('Historic_Rails');
      draw_railroad ('Active_Rails');

      bmp.Canvas.Font.Size := resource_circle_font_size;
      bmp.Canvas.Font.Family := 'Arial';
      status_circle_radius := bmp.Canvas.TextWidth ('888') * status_circle_radius_multiplier;
      ring_circle_radius := status_circle_radius * ring_circle_radius_multiplier;
      marker_radius := status_circle_radius * marker_radius_multiplier;
      for c := Low(t_resource_classification) to High(t_resource_classification) do
         for i := 1 to Length(resources)-1 do
            with resources[i] do
               if (classification = c)
                  and
                  current_page_border.contains (GISPoint(gisX, gisY))
               then
                  draw_resource_marker (id, gisx, gisy);

      bmp.Canvas.Font.Size := detail_font_size;
      bmp.Canvas.Font.Family := 'Times New Roman';
      if not detail_map then
         draw_detail_boxes;

      bmp.Canvas.Font.Size := border_font_size;
      bmp.Canvas.Font.Family := 'Times New Roman';
      draw_border;

      bmp.Canvas.EndScene;
      bmp.SaveToFile ('c:\temp\maps\map_' + map_id + '.bmp');
      bmp.Free
   end;   // draw_map_bitmap

procedure TMainForm.Button1Click(Sender: TObject);

   procedure set_parcel_color (_classification: t_resource_classification; _color: TAlphaColor);
      var
         r: integer;
         count: integer;
      begin
         count := 0;
         for r := 1 to Length(resources)-1 do
            with resources[r] do
               if (tax_parcel_idx <> 0)
                  and
                  (classification = _classification)
               then
                  begin
                     parcel_data[tax_parcel_idx].color := _color;
                     count := count + 1
                  end;
         Memo1.Lines.Add (format ('%d class %d resources', [count, ord(_classification)]))
      end;

   function GetImpactZones (classifications: t_resource_classification_set): TArcViewShapeFile;
      var
         i: integer;
         z: tHistoricalImpactZones;
      begin
         z := tHistoricalImpactZones.Create;
         for i := 0 to Length(resources)-1 do
            if resources[i].classification in classifications then
               if resources[i].non_point_resource then
                  assert (false)
               else
                  z.AddPointResource (resources[i].gisx, resources[i].gisy, 500);
         result := z.CreateHistoricalImpactZonesShapeFile;
         z.Free
      end;

   const
      layout_id = 1;
   var
      i,r,p: integer;
      pt: TGISPoint;
      muni_border_shape_file: TArcViewShapeFile;
      bmp_width_in_inches, bmp_height_in_inches: real;  // inches
      aspect_ratio: real;
      pixels_per_gis_unit: real;

   begin
      Memo1.Lines.Clear;

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

      AtlasLayoutsTable.Filter := 'LayoutId=' + IntToStr(layout_id);
      AtlasLayoutsTable.Filtered := true;
      AtlasLayoutsTable.Active := true;
      dpi := AtlasLayoutsTableDPI.AsInteger;
      border_width := AtlasLayoutsTableBorder.AsFloat;
      border_font_size := AtlasLayoutsTableBorderFontSize.AsInteger;
      resource_circle_font_size := AtlasLayoutsTableResourceFontSize.AsInteger;
      detail_font_size := AtlasLayoutsTableDetailCutoutFontSize.AsInteger;
      AtlasLayoutsTable.Active := false;

      SetLength(parcel_data, 1);  // 0th element not used
      NGParcelTable.Active := true;
      while not NGParcelTable.Eof
      do begin
            i := Length(parcel_data);
            SetLength (parcel_data, i+1);
            parcel_data[i].pin_common := NGParcelTablePIN_COMMON.AsString;
            parcel_data[i].color := TAlphaColors.White;
            NGParcelTable.Next
         end;
      NGParcelTable.Active := false;

      SetLength(resources, 1);  // 0th element not used
      ResourcesTable.Active := true;
      while not ResourcesTable.Eof
      do begin
            i := Length(resources);
            SetLength (resources, i+1);
            resources[i].id := ResourcesTableResourceId.AsInteger;
            if not resources[i].set_classification (ResourcesTableClassification.AsString) then
               Memo1.Lines.Add (format ('Resource %d: Invalid classification "%s"', [ResourcesTableResourceId.AsInteger, ResourcesTableClassification.AsString]));
            resources[i].addr911 := ResourcesTableStreetNo.AsString + ' ' + ResourcesTableStreetName.AsString;
            resources[i].gisx := ResourcesTableGISx.AsFloat;
            resources[i].gisy := ResourcesTableGISy.AsFloat;
            resources[i].non_point_resource := ResourcesTableNonPointResource.AsInteger = 1;  // can be 0, 1 or null
            resources[i].tax_parcel_idx := 0;
            ResourcesTable.Next
         end;
      ResourcesTable.Active := false;

      national_resources_impact_zones := GetImpactZones ([c_national_archaeological, c_national_nonintact, c_national_intact]);
      local_resources_impact_zones := GetImpactZones ([c_local_archaeological, c_local_nonintact, c_local_intact]);
      fifty_plus_resources_impact_zones := GetImpactZones ([c_50plus_intact]);

      muni_border_shape_file := TArcViewShapeFile.CreateFromFile ('..\db\muni_border.shp');
      assert (muni_border_shape_file.number_of_shapes = 1);
      muni_border := TArcViewPolygonShape(muni_border_shape_file[1]);
      with muni_border do
         begin
            grid_origin.x := bounding_box_Xmin - ((round(bounding_box_Xmax-bounding_box_Xmin) mod round(tic_mark_interval)) / 2);
            grid_origin.y := bounding_box_Ymax + ((round(bounding_box_Ymax-bounding_box_Ymin) mod round(tic_mark_interval)) / 2);
         end;

      current_page_border := TArcViewPolygonShape.CreateEmpty;

      parcels := TArcViewShapeFile.CreateFromFile('..\db\muni_parcels.shp');

      for r := 1 to length(resources)-1 do
         if resources[r].non_point_resource then
            assert (false)  // todo: implement polygon intersection check (eg. rr shape)
         else
            begin
               pt.x := resources[r].gisx;
               pt.y := resources[r].gisy;
               for p := 1 to Length(parcel_data)-1 do
                  if TArcViewPolygonShape(parcels[p]).contains(pt) then
                     begin
                        assert (resources[r].tax_parcel_idx = 0);
                        resources[r].tax_parcel_idx := p
                     end;
            end;
      for r := 1 to length(resources)-1 do
         if resources[r].tax_parcel_idx = 0 then
            memo1.lines.add('resource not touching a tax parcel: ' + inttostr(resources[r].id) + ' ' + inttostr(resources[r].tax_parcel_idx));
      Application.ProcessMessages;

      // class III(N) and III(A) not shown
      for p := 1 to Length(parcel_data)-1 do
         parcel_data[p].color := unclassified_color;
      set_parcel_color (c_50plus_intact, class_III_color);
      set_parcel_color (c_local_archaeological, class_II_color);
      set_parcel_color (c_local_nonintact, class_II_color);
      set_parcel_color (c_local_intact, class_II_color);
      set_parcel_color (c_national_archaeological, class_I_color);
      set_parcel_color (c_national_nonintact, class_I_color);
      set_parcel_color (c_national_intact, class_I_color);




      SetLength (bitmaps, 0);
      AtlasBitmapsTable.Filter := 'LayoutId=' + IntToStr(layout_id);
      AtlasBitmapsTable.Filtered := true;
      AtlasBitmapsTable.Active := true;
      while not AtlasBitmapsTable.Eof do
         begin
            aspect_ratio := (AtlasBitmapsTableUR_GISx.AsFloat - AtlasBitmapsTableLL_GISx.AsFloat)
                            /
                            (AtlasBitmapsTableUR_GISy.AsFloat - AtlasBitmapsTableLL_GISy.AsFloat);
            assert (Length(AtlasBitmapsTableDimensionKind.AsString) = 1);
            bmp_width_in_inches := 0; bmp_height_in_inches := 0;  // to suppress compiler warnings
            case AtlasBitmapsTableDimensionKind.AsString[1] of
               'H': begin
                       bmp_height_in_inches := AtlasBitmapsTableDimension.AsFloat;
                       bmp_width_in_inches := bmp_height_in_inches * aspect_ratio
                    end;
               'W': begin
                       bmp_width_in_inches := AtlasBitmapsTableDimension.AsFloat;
                       bmp_height_in_inches := bmp_width_in_inches / aspect_ratio
                    end;
            else
               assert (false, 'invalid TableDimensionKind field value: ' + AtlasBitmapsTableDimensionKind.AsString)
            end;

            pixels_per_gis_unit := bmp_width_in_inches * dpi / (AtlasBitmapsTableLL_GISx.AsFloat - AtlasBitmapsTableUR_GISx.AsFloat);

            i := Length (bitmaps);
            SetLength (bitmaps, i+1);
            bitmaps[i].map_id := AtlasBitmapsTableMapId.AsString;
            bitmaps[i].caption := AtlasBitmapsTableCaption.AsString;
            bitmaps[i].page_no := AtlasBitmapsTablePageNo.AsInteger;
            bitmaps[i].detail := AtlasBitmapsTableDetail.AsInteger = 1;
            bitmaps[i].ll_GISx := AtlasBitmapsTableLL_GISx.AsFloat;
            bitmaps[i].ll_GISy := AtlasBitmapsTableLL_GISy.AsFloat;
            bitmaps[i].ur_GISx := AtlasBitmapsTableUR_GISx.AsFloat;
            bitmaps[i].ur_GISy := AtlasBitmapsTableUR_GISy.AsFloat;
            bitmaps[i].bmp_width_in_pixels := round (bmp_width_in_inches * dpi);
            bitmaps[i].bmp_height_in_pixels := round (bmp_height_in_inches * dpi);

            AtlasBitmapsTable.Next
         end;
      AtlasBitmapsTable.Active := false;

      for i := 0 to Length(bitmaps)-1 do
         with bitmaps[i] do
            begin
               current_page_border.InitAsBox (GISPoint(ll_GISx,ll_GISy), GISPoint(ur_GISx, ur_GISy));
               draw_map_bitmap (map_id, bmp_width_in_pixels, bmp_height_in_pixels, detail);
               Memo1.Lines.Add ('finished bmp' + IntToStr(i));
               Application.ProcessMessages
            end;

//      ResourceMapGridLocationsTable.EmptyDataSet;
//      ResourceMapGridLocationsTable.Active := true;
//      for i := 0 to Length(resources)-1 do
//         with resources[i] do
//            begin
//               ResourceMapGridLocationsTable.Append;
//               ResourceMapGridLocationsTableResourceId.AsInteger := id;
//               ResourceMapGridLocationsTableMapGridLocation.AsString := map_grid_location;
//               ResourceMapGridLocationsTable.Post
//            end;
//      ResourceMapGridLocationsTable.Active := false;

      Memo1.Lines.Add ('DONE');

      FDConnection.Close;
      parcels.Free;
      current_page_border.Free;
      muni_border_shape_file.Free;
      national_resources_impact_zones.Free;
      local_resources_impact_zones.Free;
      fifty_plus_resources_impact_zones.Free;
   end;

procedure TMainForm.set_background_coloring_style (s: t_background_coloring_style);
   begin
      case s of
         bcs_no_coloring:
            NoColoringRadioButton.IsChecked := true;
         bcs_county_style:
            CountyStyleRadioButton.IsChecked := true;
         bcs_impact_zones:
            ImpactZonesRadioButton.IsChecked := true;
      else
         assert (false)
      end
   end;

function TMainForm.get_background_coloring_style:  t_background_coloring_style;
   begin
      if NoColoringRadioButton.IsChecked then
         result := bcs_no_coloring
      else if CountyStyleRadioButton.IsChecked then
         result := bcs_county_style
      else if ImpactZonesRadioButton.IsChecked then
         result := bcs_impact_zones
      else
         assert (false)
   end;

END.
