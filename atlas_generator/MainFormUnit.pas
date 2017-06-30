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
   //   this color combination is “colorblind safe”, “printer friendly” and “photocopy safe”.
   class_I_color      = TAlphaColor($FFfd301f);
   class_II_color     = TAlphaColor($FFfc8d59);
   class_III_color    = TAlphaColor($FFfdcc8a);
   unclassified_color = TAlphaColor($FFfef0d9);

   intact_color = TAlphaColors.Green;
   nonintact_color = TAlphaColors.Yellow;
   archaeological_site_color = TAlphaColors.Darkslategray;

   // constants for drawing resource id circles
   status_circle_radius_multiplier = 0.4875;
   ring_circle_radius_multiplier = 1.15;
   marker_radius_multiplier = 1.25;

   tie_width_dpi_divisor = 10;
   tie_spacing_dpi_divisor = 5;

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

type
   t_ordinance_resource =
      record
         id: integer;
//            pin: string;
         gisx, gisy: real;
         non_point_resource: boolean;
//            map_page, photo_page: integer;
//            photo_fn: string;
         addr911: string;
         tax_parcel_idx: integer;
         ring_color: TAlphaColor;
         status_color: TAlphaColor;
         classification: t_resource_classification;
         function set_classification (s: string): boolean;
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
    procedure Button1Click(Sender: TObject);
  private
      parcels: TArcViewShapeFile;
      muni_border: TArcViewPolygonShape;
      current_page_border: TArcViewPolygonShape;
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
               page_no: integer;
               detail: boolean;
               ll_GISx: real;
               ll_GISy: real;
               ur_GISx: real;
               ur_GISy: real;
               bmp_width_in_pixels: integer;
               bmp_height_in_pixels: integer
            end;
      resources: array of t_ordinance_resource;
      dpi: integer;
      resource_circle_font_size: integer;
      detail_font_size: integer;

    procedure draw_map_bitmap (bmp_no: integer; bmp_image_width_in_pixels, bmp_image_height_in_pixels: integer; detail_map: boolean);
  public
    { Public declarations }
  end;

var
   MainForm: TMainForm;

IMPLEMENTATION

{$R *.fmx}

uses
   Utils, FMX.Printer, Winapi.Windows, System.UIConsts, fmx.objects,
   System.Math;

var
   activated: boolean;

function t_ordinance_resource.set_classification (s: string): boolean;
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

procedure TMainForm.draw_map_bitmap (bmp_no: integer; bmp_image_width_in_pixels, bmp_image_height_in_pixels: integer; detail_map: boolean);
   var
      bmp: FMX.Graphics.TBitmap;
      pixels_per_gis_unit: real;
      marker_radius: real;
      ring_circle_radius: real;
      status_circle_radius: real;

   function pixelX (gisX: real): integer;   // convert gisX to pixelX on current page
      begin
         pixelX := round ((gisX - current_page_border.Part[0,0].x) * pixels_per_gis_unit)
      end;

   function pixelY (gisY: real): integer;   // convert gisY to pixelY on current page
      begin
         result := round ((current_page_border.Part[0,0].y - gisY) * pixels_per_gis_unit)
      end;

   procedure draw_polygons (polys: TArcViewShapeFile; color_parcel: boolean);
      var
         p,i: integer;
         parcel_no: integer;
         points: TPolygon;  //array of TPoint;
         intersection: TArcViewPolygonShape;
      begin
         for parcel_no := 1 to polys.number_of_shapes do
            begin
               intersection := TArcViewPolygonShape(polys.shapes [parcel_no]).Intersection(muni_border);
               with intersection do
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
                        if color_parcel
                           and
                           (parcel_data[parcel_no].color <> TAlphaColors.white)
                        then
                           begin
                              bmp.Canvas.Fill.Color := parcel_data[parcel_no].color;
                              bmp.Canvas.FillPolygon (points, 1);
                              bmp.Canvas.Fill.Color := TAlphaColors.white
                           end;
                        bmp.Canvas.DrawPolygon (points, 1)
                     end;
               intersection.Free
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
         gispt: TGISPoint;
         pt1, pt2: TPointF;
         old_stroke_thickness: single;
      begin   // draw_resource_marker
         gispt.X := gisx;
         gispt.Y := gisy;
         if current_page_border.contains (gispt) then
            begin
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
            end
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

   var
      i: integer;
      c: t_resource_classification;
   begin    // draw_map_bitmap
      pixels_per_gis_unit := bmp_image_width_in_pixels / current_page_border.width;

      bmp := FMX.Graphics.TBitmap.Create(bmp_image_width_in_pixels, bmp_image_height_in_pixels);
      bmp.Canvas.BeginScene;
      bmp.Canvas.Clear(TAlphaColors.White);

      draw_polygons (parcels, true);
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
               if classification = c then
                  draw_resource_marker (id, gisx, gisy);

      bmp.Canvas.Font.Size := detail_font_size;
      bmp.Canvas.Font.Family := 'Times New Roman';
      if not detail_map then
         draw_detail_boxes;

      bmp.Canvas.EndScene;
      bmp.SaveToFile ('c:\temp\maps\map_' + IntToStr(bmp_no) + '.bmp');
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
      if activated then
         exit;
      activated := true;

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

      muni_border_shape_file := TArcViewShapeFile.CreateFromFile ('..\db\muni_border.shp');
      assert (muni_border_shape_file.number_of_shapes = 1);
      muni_border := TArcViewPolygonShape(muni_border_shape_file[1]);

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

      AtlasLayoutsTable.Filter := 'LayoutId=' + IntToStr(layout_id);
      AtlasLayoutsTable.Filtered := true;
      AtlasLayoutsTable.Active := true;
      dpi := AtlasLayoutsTableDPI.AsInteger;
      resource_circle_font_size := AtlasLayoutsTableResourceFontSize.AsInteger;
      detail_font_size := AtlasLayoutsTableDetailCutoutFontSize.AsInteger;
      AtlasLayoutsTable.Active := false;

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
               draw_map_bitmap (i, bmp_width_in_pixels, bmp_height_in_pixels, detail)
            end;

      FDConnection.Close;
      parcels.Free;
      current_page_border.Free;
      muni_border_shape_file.Free
   end;

END.
