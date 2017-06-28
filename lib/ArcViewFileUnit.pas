UNIT ArcViewFileUnit;

//=================================================================
//
//  REFERENCE: ESRI Shapefile Technical Description
//     https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf
//
//  This is only a partial implementation of the ESRI Shapefile
//  specifications.  Additional functionality is begin added only
//  as needed.
//
//=================================================================

INTERFACE

uses
   clipper;


const
   null_shape_type      =  0;
   point_shape_type     =  1;
   polyline_shape_type  =  3;
   polygon_shape_type   =  5;
   polylineZ_shape_type = 13;

type
   TArcViewShapeFile = class;
   TArcViewShapeHeader = class;
   TArcViewShapeBaseClass = class;
   TArcViewShapeWithBoundingBoxBaseClass = class;
   TArcViewPolygonShape = class;

   TGISPoint =
      record
         x,y: real;
      end;

   TGISPointArray =
      array of TGISPoint;

   TByteArray =
      class
         destructor Destroy;
            override;
      protected
         constructor Create (size: integer);
         constructor CreateCopy (ba: TByteArray);
         procedure read_from_file (var f: file; size: integer);
         procedure write_to_file (var f: file);
            virtual;
         function size: integer;
         function r_big_end_integer (idx: integer): integer;
            virtual;
         function r_big_end_double (idx: integer): double;
            virtual;
         function r_little_end_integer (idx: integer): integer;
            virtual;
         function r_little_end_double (idx: integer): double;
            virtual;
         procedure w_big_end_integer (val, idx: integer);
            virtual;
         procedure w_big_end_double (val: double; idx: integer);
            virtual;
         procedure w_little_end_integer (val, idx: integer);
            virtual;
         procedure w_little_end_double (val: double; idx: integer);
            virtual;
      private
         b: array of byte;
      end;

   TArcViewFileHeader =
      class (TByteArray)
         constructor Create (shape_type: integer);
         constructor CreateFromFile (var f: file);
         destructor Destroy;
            override;
      private
         function r_file_code: integer;
         function r_file_length: integer;
         function r_version: integer;
         function r_shape_type: integer;
         function r_bounding_box_Xmin: double;
         function r_bounding_box_Ymin: double;
         function r_bounding_box_Xmax: double;
         function r_bounding_box_Ymax: double;
         function r_bounding_box_Zmin: double;
         function r_bounding_box_Zmax: double;
         function r_bounding_box_Mmin: double;
         function r_bounding_box_Mmax: double;
         procedure w_file_code (val: integer);
         procedure w_file_length (val: integer);
         procedure w_version (val: integer);
         procedure w_bounding_box_Xmin (val: double);
         procedure w_bounding_box_Ymin (val: double);
         procedure w_bounding_box_Xmax (val: double);
         procedure w_bounding_box_Ymax (val: double);
         procedure w_bounding_box_Zmin (val: double);
         procedure w_bounding_box_Zmax (val: double);
         procedure w_bounding_box_Mmin (val: double);
         procedure w_bounding_box_Mmax (val: double);
      public
         bounding_box: TArcViewPolygonShape;   // only available if created from file
         property file_code: integer read r_file_code write w_file_code;
         property file_length: integer read r_file_length write w_file_length;
         property version: integer read r_version write w_version;
         property shape_type: integer read r_shape_type;
         property bounding_box_Xmin: double read r_bounding_box_Xmin write w_bounding_box_Xmin;
         property bounding_box_Ymin: double read r_bounding_box_Ymin write w_bounding_box_Ymin;
         property bounding_box_Xmax: double read r_bounding_box_Xmax write w_bounding_box_Xmax;
         property bounding_box_Ymax: double read r_bounding_box_Ymax write w_bounding_box_Ymax;
         property bounding_box_Zmin: double read r_bounding_box_Zmin write w_bounding_box_Zmin;
         property bounding_box_Zmax: double read r_bounding_box_Zmax write w_bounding_box_Zmax;
         property bounding_box_Mmin: double read r_bounding_box_Mmin write w_bounding_box_Mmin;
         property bounding_box_Mmax: double read r_bounding_box_Mmax write w_bounding_box_Mmax;
         procedure initialize_bounding_box (pt: TGISPoint);
            overload;
         procedure include_in_bounding_box (pt: TGISPoint);
            overload;
         procedure initialize_bounding_box (sh: TArcViewShapeWithBoundingBoxBaseClass);
            overload;
         procedure include_in_bounding_box (sh: TArcViewShapeWithBoundingBoxBaseClass);
            overload;
      end;

   TArcViewShapeHeader =
      class (TByteArray)
         constructor Create (record_number: integer; shape: TArcViewShapeBaseClass);
         constructor CreateFromFile (var f: file);
      private
         function r_record_number: integer;
         function r_content_length: integer;
         function r_shape_type: integer;
         procedure w_record_number (val: integer);
         procedure w_content_length (val: integer);
         procedure w_shape_type (val: integer);
      public
         property record_number: integer read r_record_number write w_record_number;
         property content_length: integer read r_content_length write w_content_length;
         property shape_type: integer read r_shape_type write w_shape_type;
      end;

   TArcViewShapeBaseClass =   // base classes for shapes
      class (TByteArray)
      protected
         constructor CreateFromFile (var f: file; content_length: integer);
            virtual;
         function shape_type: integer;
            virtual; abstract;
         function r_big_end_integer (idx: integer): integer;
            override;
         function r_big_end_double (idx: integer): double;
            override;
         function r_little_end_integer (idx: integer): integer;
            override;
         function r_little_end_double (idx: integer): double;
            override;
         procedure w_big_end_integer (val, idx: integer);
            override;
         procedure w_big_end_double (val: double; idx: integer);
            override;
         procedure w_little_end_integer (val, idx: integer);
            override;
         procedure w_little_end_double (val: double; idx: integer);
            override;
      public
         function CreateCopy: TArcViewShapeBaseClass;
            virtual; abstract;
      end;

   TArcViewShapeWithBoundingBoxBaseClass =  // base class only
      class (TArcViewShapeBaseClass)
      private
         procedure w_bounding_box_Xmin (val: double);
         procedure w_bounding_box_Ymin (val: double);
         procedure w_bounding_box_Xmax (val: double);
         procedure w_bounding_box_Ymax (val: double);
         function r_bounding_box_Xmin: double;
         function r_bounding_box_Ymin: double;
         function r_bounding_box_Xmax: double;
         function r_bounding_box_Ymax: double;
      public
         bounding_box: TArcViewPolygonShape;   // only available if read from file
         constructor CreateFromFile (var f: file; content_length: integer);
            override;
         property bounding_box_Xmin: double read r_bounding_box_Xmin write w_bounding_box_Xmin;
         property bounding_box_Ymin: double read r_bounding_box_Ymin write w_bounding_box_Ymin;
         property bounding_box_Xmax: double read r_bounding_box_Xmax write w_bounding_box_Xmax;
         property bounding_box_Ymax: double read r_bounding_box_Ymax write w_bounding_box_Ymax;
         destructor Destroy;
            override;
      protected
         procedure initialize_bounding_box (pt: TGISPoint);
         procedure include_in_bounding_box (pt: TGISPoint);
      end;

   TArcViewNullShape =
      class (TArcViewShapeBaseClass)
         function shape_type: integer;
            override;
         procedure write_to_file (var f: file);
            override;
      end;

   TArcViewPointShape =
      class (TArcViewShapeBaseClass)
         constructor Create (pt: TGISPoint);
         function shape_type: integer;
            override;
         function point: TGISPoint;
         function CreateCopy: TArcViewShapeBaseClass;
            override;
         procedure write_to_file (var f: file);
            override;
      end;

   TArcViewPolyLineShape =
      class (TArcViewShapeWithBoundingBoxBaseClass)
      const
         iScale = 12.0;   // converts feet to inches in the projection I am using
      var
         iPart: TPaths;  // in integer iScale*UTM (0.1 meter)
         Part: array of TGISPointArray;  // real in UTM units (1 meter)
         constructor CreateFromPolyLineShapeFile (var f: file; content_length: integer);
         function shape_type: integer;
            override;
         destructor Destroy;
            override;
         function number_of_parts: integer;
         function CreateCopy: TArcViewShapeBaseClass;
            override;
         procedure write_to_file (var f: file);
            override;
      private
         function part_point_index (part_no: integer): integer;
      end;

   TArcViewPolyLineZShape =
      class (TArcViewPolyLineShape)
         // Z information not implemented
         constructor CreateFromPolyLineZShapeFile (var f: file; content_length: integer);
         function shape_type: integer;
            override;
      end;

   TArcViewPolygonShape =
      class (TArcViewPolyLineShape)
         constructor CreateEmpty;
         procedure Clear;
         procedure Add (pt: TGISPoint);
            overload;
         procedure Add (x, y: real);
            overload;
         function CreateCopy: TArcViewShapeBaseClass;
            override;
         function shape_type: integer;
            override;
         function area: real;  // in sq-meters
         function distance_from_boundary (pt: TGISPoint): real;  // in meters
            // returns positive number if outside, negative number if inside
         function contains (pt: TGISPoint): boolean;
         function Intersects (poly: TArcViewPolygonShape): boolean;
         function Intersection (poly: TArcViewPolygonShape): TArcViewPolygonShape;
      private
         function polygon_area (const polygon: TGISPointArray): real;
            // result in sq-GIS units
            // positive result for clockwise polygons,
            // negative result for counter-clockwise polygon
      end;

   TArcViewShapeFile =
      class
      private
         f_number_of_shapes: integer;
         f_shape_headers: array of TArcViewShapeHeader; // 1..number_of_shapes (0th location not used)
         f_shapes: array of TArcViewShapeBaseClass;  // 1..number_of_shapes (0th location not used)
         procedure w_number_of_shapes (val: integer);
         function r_shape (idx: integer): TArcViewShapeBaseClass;
         procedure w_shape (idx: integer; val: TArcViewShapeBaseClass);
      public
         file_header: TArcViewFileHeader;
         property number_of_shapes: integer read f_number_of_shapes write w_number_of_shapes;
         property shapes [idx: integer]: TArcViewShapeBaseClass read r_shape write w_shape; default;
         constructor Create (shape_type: integer);
         constructor CreateFromFile (file_name: string);
         procedure Add (shape: TArcViewShapeBaseClass);
         destructor Destroy;
            override;
         procedure WriteToFile (file_name: string);
      end;

IMPLEMENTATION

uses SysUtils;

type
   TArcViewIndexRecord =
      class (TByteArray)
         constructor CreateFromFile (var f: file);
         constructor Create (offset, content_length: integer);
      private
         function r_offset: integer;
         function r_content_length: integer;
         procedure w_offset (val: integer);
         procedure w_content_length (val: integer);
      public
         property offset: integer read r_offset write w_offset;
         property content_length: integer read r_content_length write w_content_length;
      end;

   TArcViewIndexFile =
      class
      private
         f_file_header: TArcViewFileHeader;
         f_number_of_records: integer;
         f_index_record: array of TArcViewIndexRecord;  // 1..number_of_records (0th location not used)
         function r_index_record (idx: integer): TArcViewIndexRecord;
      public
         property file_header: TArcViewFileHeader read f_file_header;
         property number_of_records: integer read f_number_of_records;
         property index_record [idx: integer]: TArcViewIndexRecord read r_index_record;  // 1..number_of_records (0th location not used)
         constructor CreateFromFile (file_name: string);
         constructor CreateFromTArcViewShapeFile (f: TArcViewShapeFile);
         procedure WriteToFile (file_name: string);
         destructor Destroy;
            override;
      end;


//==============================================================================
// TByteArray

constructor TByteArray.Create (size: integer);
   var
      i: integer;
   begin
      SetLength (b, size);
      for i := 0 to size-1
      do b[i] := 0
   end;

constructor TByteArray.CreateCopy (ba: TByteArray);
   var
      i: integer;
   begin
      SetLength (b, ba.size);
      for i := 0 to ba.size-1
      do b[i] := ba.b[i]
   end;

procedure TByteArray.read_from_file (var f: file; size: integer);
   var
      bytes_read: integer;
   begin
      BlockRead (f, b[0], size, bytes_read);
      assert (bytes_read = size)
   end;

procedure TByteArray.write_to_file (var f: file);
   begin
      BlockWrite (f, b[0], Length(b))
   end;

destructor TByteArray.Destroy;
   begin
      b := nil
   end;

function TByteArray.size: integer;
   begin
      result := Length(b)
   end;

function TByteArray.r_big_end_integer (idx: integer): integer;
   var
      vr: record
             case integer of
                0: (i: longint);
                1: (b3,b2,b1,b0: byte)
             end;
   begin
      vr.b0 := b[idx];
      vr.b1 := b[idx+1];
      vr.b2 := b[idx+2];
      vr.b3 := b[idx+3];
      result := vr.i
   end;

function TByteArray.r_big_end_double (idx: integer): double;
   var
      vr: record
             case integer of
                0: (d: double);
                1: (b7,b6,b5,b4,b3,b2,b1,b0: byte)
             end;
   begin
      vr.b0 := b[idx];
      vr.b1 := b[idx+1];
      vr.b2 := b[idx+2];
      vr.b3 := b[idx+3];
      vr.b4 := b[idx+4];
      vr.b5 := b[idx+5];
      vr.b6 := b[idx+6];
      vr.b7 := b[idx+7];
      result := vr.d
   end;

function TByteArray.r_little_end_integer (idx: integer): integer;
   var
      vr: record
             case integer of
                0: (i: longint);
                1: (b0,b1,b2,b3: byte)
             end;
   begin
      vr.b0 := b[idx];
      vr.b1 := b[idx+1];
      vr.b2 := b[idx+2];
      vr.b3 := b[idx+3];
      result := vr.i
   end;

function TByteArray.r_little_end_double (idx: integer): double;
   var
      vr: record
             case integer of
                0: (d: double);
                1: (b0,b1,b2,b3,b4,b5,b6,b7: byte)
             end;
   begin
      vr.b0 := b[idx];
      vr.b1 := b[idx+1];
      vr.b2 := b[idx+2];
      vr.b3 := b[idx+3];
      vr.b4 := b[idx+4];
      vr.b5 := b[idx+5];
      vr.b6 := b[idx+6];
      vr.b7 := b[idx+7];
      result := vr.d
   end;

procedure TByteArray.w_big_end_integer (val, idx: integer);
   var
      vr: record
             case integer of
                0: (i: longint);
                1: (b3,b2,b1,b0: byte)
             end;
   begin
      vr.i := val;
      b[idx] := vr.b0;
      b[idx+1] := vr.b1;
      b[idx+2] := vr.b2;
      b[idx+3] := vr.b3
   end;

procedure TByteArray.w_big_end_double (val: double; idx: integer);
   var
      vr: record
             case integer of
                0: (d: double);
                1: (b7,b6,b5,b4,b3,b2,b1,b0: byte)
             end;
   begin
      vr.d := val;
      b[idx] := vr.b0;
      b[idx+1] := vr.b1;
      b[idx+2] := vr.b2;
      b[idx+3] := vr.b3;
      b[idx+4] := vr.b4;
      b[idx+5] := vr.b5;
      b[idx+6] := vr.b6;
      b[idx+7] := vr.b7
   end;

procedure TByteArray.w_little_end_integer (val, idx: integer);
   var
      vr: record
             case integer of
                0: (i: longint);
                1: (b0,b1,b2,b3: byte)
             end;
   begin
      vr.i := val;
      b[idx] := vr.b0;
      b[idx+1] := vr.b1;
      b[idx+2] := vr.b2;
      b[idx+3] := vr.b3
   end;

procedure TByteArray.w_little_end_double (val: double; idx: integer);
   var
      vr: record
             case integer of
                0: (d: double);
                1: (b0,b1,b2,b3,b4,b5,b6,b7: byte)
             end;
   begin
      vr.d := val;
      b[idx] := vr.b0;
      b[idx+1] := vr.b1;
      b[idx+2] := vr.b2;
      b[idx+3] := vr.b3;
      b[idx+4] := vr.b4;
      b[idx+5] := vr.b5;
      b[idx+6] := vr.b6;
      b[idx+7] := vr.b7
   end;


//==============================================================================
//  TArcViewFileHeader

constructor TArcViewFileHeader.Create (shape_type: integer);
   begin
      inherited Create (100);
      w_big_end_integer (9994, 0);
      w_little_end_integer (1000, 28);
      w_little_end_integer (shape_type, 32)
   end;

constructor TArcViewFileHeader.CreateFromFile (var f: file);
   begin
      inherited Create (100);
      read_from_file (f, 100);
      bounding_box := TArcViewPolygonShape.CreateEmpty;
      bounding_box.Add (bounding_box_Xmin, bounding_box_Ymin);
      bounding_box.Add (bounding_box_Xmin, bounding_box_Ymax);
      bounding_box.Add (bounding_box_Xmax, bounding_box_Ymax);
      bounding_box.Add (bounding_box_Xmax, bounding_box_Ymin);
      bounding_box.Add (bounding_box_Xmin, bounding_box_Ymin)
   end;

destructor TArcViewFileHeader.Destroy;
   begin
      bounding_box.Free;
      inherited
   end;

function TArcViewFileHeader.r_file_code: integer;
   begin
      result := r_big_end_integer(0)
   end;

procedure TArcViewFileHeader.w_file_code (val: integer);
   begin
      w_big_end_integer(val, 0)
   end;

function TArcViewFileHeader.r_file_length: integer;
   begin
      result := r_big_end_integer(24)
   end;

procedure TArcViewFileHeader.w_file_length (val: integer);
   begin
      w_big_end_integer(val, 24)
   end;

function TArcViewFileHeader.r_version: integer;
   begin
      result := r_little_end_integer(28)
   end;

procedure TArcViewFileHeader.w_version (val: integer);
   begin
      w_little_end_integer(val, 28)
   end;

function TArcViewFileHeader.r_shape_type: integer;
   begin
      result := r_little_end_integer(32)
   end;

function TArcViewFileHeader.r_bounding_box_Xmin: double;
   begin
      result := r_little_end_double(36)
   end;

procedure TArcViewFileHeader.w_bounding_box_Xmin (val: double);
   begin
      w_little_end_double(val, 36)
   end;

function TArcViewFileHeader.r_bounding_box_Ymin: double;
   begin
      result := r_little_end_double(44)
   end;

procedure TArcViewFileHeader.w_bounding_box_Ymin (val: double);
   begin
      w_little_end_double(val, 44)
   end;

function TArcViewFileHeader.r_bounding_box_Xmax: double;
   begin
      result := r_little_end_double(52)
   end;

procedure TArcViewFileHeader.w_bounding_box_Xmax (val: double);
   begin
      w_little_end_double(val, 52)
   end;

function TArcViewFileHeader.r_bounding_box_Ymax: double;
   begin
      result := r_little_end_double(60)
   end;

procedure TArcViewFileHeader.w_bounding_box_Ymax (val: double);
   begin
      w_little_end_double(val, 60)
   end;

function TArcViewFileHeader.r_bounding_box_Zmin: double;
   begin
      result := r_little_end_double(68)
   end;

procedure TArcViewFileHeader.w_bounding_box_Zmin (val: double);
   begin
      w_little_end_double(val, 68)
   end;

function TArcViewFileHeader.r_bounding_box_Zmax: double;
   begin
      result := r_little_end_double(76)
   end;

procedure TArcViewFileHeader.w_bounding_box_Zmax (val: double);
   begin
      w_little_end_double(val, 76)
   end;

function TArcViewFileHeader.r_bounding_box_Mmin: double;
   begin
      result := r_little_end_double(84)
   end;

procedure TArcViewFileHeader.w_bounding_box_Mmin (val: double);
   begin
      w_little_end_double(val, 84)
   end;

function TArcViewFileHeader.r_bounding_box_Mmax: double;
   begin
      result := r_little_end_double(92)
   end;

procedure TArcViewFileHeader.w_bounding_box_Mmax (val: double);
   begin
      w_little_end_double(val, 92)
   end;

procedure TArcViewFileHeader.initialize_bounding_box (pt: TGISPoint);
   begin
      w_bounding_box_Xmin (pt.x);
      w_bounding_box_Xmax (pt.x);
      w_bounding_box_Ymin (pt.y);
      w_bounding_box_Ymax (pt.y)
   end;

procedure TArcViewFileHeader.include_in_bounding_box (pt: TGISPoint);
   begin
      if pt.x < bounding_box_Xmin
      then
         w_bounding_box_Xmin (pt.x)
      else
         if pt.x > bounding_box_Xmax
         then
            w_bounding_box_Xmax (pt.x);
      if pt.y < bounding_box_Ymin
      then
         w_bounding_box_Ymin (pt.y)
      else
         if pt.y > bounding_box_Ymax
         then
            w_bounding_box_Ymax (pt.y)
   end;

procedure TArcViewFileHeader.initialize_bounding_box (sh: TArcViewShapeWithBoundingBoxBaseClass);
   begin
      w_bounding_box_Xmin (sh.bounding_box_Xmin);
      w_bounding_box_Xmax (sh.bounding_box_Xmax);
      w_bounding_box_Ymin (sh.bounding_box_Ymin);
      w_bounding_box_Ymax (sh.bounding_box_Ymax)
   end;

procedure TArcViewFileHeader.include_in_bounding_box (sh: TArcViewShapeWithBoundingBoxBaseClass);
   begin
      if sh.bounding_box_Xmin < bounding_box_Xmin then
         w_bounding_box_Xmin (sh.bounding_box_Xmin);
      if sh.bounding_box_Xmax > bounding_box_Xmax then
         w_bounding_box_Xmax (sh.bounding_box_Xmax);
      if sh.bounding_box_Ymin < bounding_box_Ymin then
         w_bounding_box_Ymin (sh.bounding_box_Ymin);
      if sh.bounding_box_Ymax > bounding_box_Ymax then
         w_bounding_box_Ymax (sh.bounding_box_Ymax)
   end;


//==============================================================================
// TArcViewIndexRecord

constructor TArcViewIndexRecord.CreateFromFile (var f: file);
   begin
      inherited Create (8);
      read_from_file (f, 8)
   end;

constructor TArcViewIndexRecord.Create (offset, content_length: integer);
   begin
      inherited Create (8);
      w_offset (offset);
      w_content_length (content_length)
   end;

function TArcViewIndexRecord.r_offset: integer;
   begin
      result := r_big_end_integer(0)
   end;

procedure TArcViewIndexRecord.w_offset (val: integer);
   begin
      w_big_end_integer(val, 0)
   end;

function TArcViewIndexRecord.r_content_length: integer;
   begin
      result := r_big_end_integer(4)
   end;

procedure TArcViewIndexRecord.w_content_length (val: integer);
   begin
      w_big_end_integer(val, 4)
   end;


//==============================================================================
// TArcViewIndexFile

constructor TArcViewIndexFile.CreateFromTArcViewShapeFile (f: TArcViewShapeFile);
   var
      i: integer;
      offset: integer;
   begin
      f_file_header := TArcViewFileHeader.CreateCopy (f.file_header);
      file_header.file_length := 50 + (f.number_of_shapes*4);
      f_number_of_records := f.number_of_shapes;
      SetLength (f_index_record, number_of_records+1);  // index_record[0] not used
      offset := 50;
      for i := 1 to number_of_records
      do begin
            f_index_record[i] := TArcViewIndexRecord.Create(offset, f.f_shape_headers[i].content_length);
            offset := offset + 4 + f.f_shape_headers[i].content_length
         end
   end;

constructor TArcViewIndexFile.CreateFromFile (file_name: string);
   var
      i: integer;
      f: file;
   begin
      assignfile (f, file_name);
      reset (f, 1);
      f_file_header := TArcViewFileHeader.CreateFromFile (f);
      f_number_of_records := (file_header.file_length-50) div 4;
      SetLength (f_index_record, number_of_records + 1);   // index_record[0] not used
      for i := 1 to number_of_records
      do f_index_record[i] := TArcViewIndexRecord.CreateFromFile(f);
      closefile (f)
   end;

function TArcViewIndexFile.r_index_record (idx: integer): TArcViewIndexRecord;
   begin
      result := f_index_record [idx]
   end;

procedure TArcViewIndexFile.WriteToFile (file_name: string);
   var
      i: integer;
      f: file;
   begin
      assignfile (f, file_name);
      rewrite (f, 1);
      file_header.write_to_file (f);
      for i := 1 to number_of_records
      do index_record[i].write_to_file (f);
      closefile (f)
   end;

destructor TArcViewIndexFile.Destroy;
   var i: integer;
   begin
      file_header.Free;
      for i := 1 to number_of_records
      do index_record[i].Free
   end;

// Some adjustments are needed since the shape type is read in as part
// of the shape header instead of the shape.  The consequence is that four extra
// bytes are read in as part of the header, and four less as part of the shape.
// All shape field positions are therefore adjusted downward by four bytes.
// The later adjustment is handled by new virtual functions in TArcViewShapeBaseClass for
// reading and writing from the underlying byte array, thus allowing the position
// values from the documentation to be used in TArcViewShapeBaseClass's descendents.
const shape_type_moved_adjustment = 4;

// TArcViewShapeHeader

constructor TArcViewShapeHeader.Create (record_number: integer; shape: TArcViewShapeBaseClass);
   begin
      inherited Create (8 + shape_type_moved_adjustment);
      w_record_number (record_number);
      w_content_length ((shape.size + shape_type_moved_adjustment) div 2);
      w_shape_type (shape.shape_type)
   end;

constructor TArcViewShapeHeader.CreateFromFile (var f: file);
   begin
      inherited Create (8 + shape_type_moved_adjustment);
      read_from_file (f, 8 + shape_type_moved_adjustment)
   end;

function TArcViewShapeHeader.r_record_number: integer;
   begin
      result := r_big_end_integer(0)
   end;

procedure TArcViewShapeHeader.w_record_number (val: integer);
   begin
      w_big_end_integer(val, 0)
   end;

function TArcViewShapeHeader.r_content_length: integer;
   begin
      result := r_big_end_integer(4)
   end;

procedure TArcViewShapeHeader.w_content_length (val: integer);
   begin
      w_big_end_integer(val, 4)
   end;

function TArcViewShapeHeader.r_shape_type: integer;
   begin
      result := r_little_end_integer(8)
   end;

procedure TArcViewShapeHeader.w_shape_type (val: integer);
   begin
      w_little_end_integer(val, 8)
   end;


//==============================================================================
// TArcViewShapeBaseClass

//constructor TArcViewShapeBaseClass.Create (size: integer);
//   begin
//      inherited Create (size - shape_type_moved_adjustment)
//   end;

constructor TArcViewShapeBaseClass.CreateFromFile (var f: file; content_length: integer);
   begin
      inherited Create ((content_length*2) - shape_type_moved_adjustment);
      read_from_file (f, (content_length*2) - shape_type_moved_adjustment)
   end;

//constructor TArcViewShapeBaseClass.CreateCopy (shape: TArcViewShapeBaseClass);
//   var
//      i: integer;
//   begin
//      header := shape.header;
//      SetLength (b, Length(shape.b));
//      for i := 0 to Length(b)-1 do
//         b[i] := shape.b[i]
//   end;

function TArcViewShapeBaseClass.r_big_end_integer (idx: integer): integer;
   begin
      result := inherited r_big_end_integer (idx-shape_type_moved_adjustment)
   end;

function TArcViewShapeBaseClass.r_big_end_double (idx: integer): double;
   begin
      result := inherited r_big_end_double (idx-shape_type_moved_adjustment)
   end;

function TArcViewShapeBaseClass.r_little_end_integer (idx: integer): integer;
   begin
      result := inherited r_little_end_integer (idx-shape_type_moved_adjustment)
   end;

function TArcViewShapeBaseClass.r_little_end_double (idx: integer): double;
   begin
      result := inherited r_little_end_double (idx-shape_type_moved_adjustment)
   end;

procedure TArcViewShapeBaseClass.w_big_end_integer (val, idx: integer);
   begin
      inherited w_big_end_integer (val, idx-shape_type_moved_adjustment)
   end;

procedure TArcViewShapeBaseClass.w_big_end_double (val: double; idx: integer);
   begin
      inherited w_big_end_double (val, idx-shape_type_moved_adjustment)
   end;

procedure TArcViewShapeBaseClass.w_little_end_integer (val, idx: integer);
   begin
      inherited w_little_end_integer (val, idx-shape_type_moved_adjustment)
   end;

procedure TArcViewShapeBaseClass.w_little_end_double (val: double; idx: integer);
   begin
      inherited w_little_end_double (val, idx-shape_type_moved_adjustment)
   end;


// TArcViewShapeWithBoundingBoxBaseClass

constructor TArcViewShapeWithBoundingBoxBaseClass.CreateFromFile (var f: file; content_length: integer);
   begin
      inherited;
      bounding_box := TArcViewPolygonShape.CreateEmpty;
      bounding_box.Add (bounding_box_Xmin, bounding_box_Ymin);
      bounding_box.Add (bounding_box_Xmin, bounding_box_Ymax);
      bounding_box.Add (bounding_box_Xmax, bounding_box_Ymax);
      bounding_box.Add (bounding_box_Xmax, bounding_box_Ymin);
      bounding_box.Add (bounding_box_Xmin, bounding_box_Ymin)
   end;

destructor TArcViewShapeWithBoundingBoxBaseClass.Destroy;
   begin
      bounding_box.Free;
      inherited
   end;

function TArcViewShapeWithBoundingBoxBaseClass.r_bounding_box_Xmin: double;
   begin
      result := r_little_end_double (4)
   end;

procedure TArcViewShapeWithBoundingBoxBaseClass.w_bounding_box_Xmin (val: double);
   begin
      w_little_end_double (val, 4)
   end;

function TArcViewShapeWithBoundingBoxBaseClass.r_bounding_box_Ymin: double;
   begin
      result := r_little_end_double (12)
   end;

procedure TArcViewShapeWithBoundingBoxBaseClass.w_bounding_box_Ymin (val: double);
   begin
      w_little_end_double (val, 12)
   end;

function TArcViewShapeWithBoundingBoxBaseClass.r_bounding_box_Xmax: double;
   begin
      result := r_little_end_double (20)
   end;

procedure TArcViewShapeWithBoundingBoxBaseClass.w_bounding_box_Xmax (val: double);
   begin
      w_little_end_double (val, 20)
   end;

function TArcViewShapeWithBoundingBoxBaseClass.r_bounding_box_Ymax: double;
   begin
      result := r_little_end_double (28)
   end;

procedure TArcViewShapeWithBoundingBoxBaseClass.w_bounding_box_Ymax (val: double);
   begin
      w_little_end_double (val, 28)
   end;

procedure TArcViewShapeWithBoundingBoxBaseClass.initialize_bounding_box (pt: TGISPoint);
   begin
      w_bounding_box_Xmin (pt.x);
      w_bounding_box_Xmax (pt.x);
      w_bounding_box_Ymin (pt.y);
      w_bounding_box_Ymax (pt.y)
   end;

procedure TArcViewShapeWithBoundingBoxBaseClass.include_in_bounding_box (pt: TGISPoint);
   begin
      if pt.x < bounding_box_Xmin
      then
         w_bounding_box_Xmin (pt.x)
      else
         if pt.x > bounding_box_Xmax
         then
            w_bounding_box_Xmax (pt.x);
      if pt.y < bounding_box_Ymin
      then
         w_bounding_box_Ymin (pt.y)
      else
         if pt.y > bounding_box_Ymax
         then
            w_bounding_box_Ymax (pt.y)
   end;


//==============================================================================
// TArcViewNullShape

function TArcViewNullShape.shape_type: integer;
   begin
      result := 0
   end;

procedure TArcViewNullShape.write_to_file (var f: file);
   begin
      assert (false, 'not implemented')
   end;


//==============================================================================
// TArcViewPointShape

constructor TArcViewPointShape.Create (pt: TGISPoint);
   begin
      inherited Create (20);
      w_little_end_double (pt.x, 4);
      w_little_end_double (pt.y, 12)
   end;

function TArcViewPointShape.CreateCopy: TArcViewShapeBaseClass;
   begin
      result := TArcViewPointShape.Create (point)
   end;

function TArcViewPointShape.shape_type: integer;
   begin
      result := 1
   end;

function TArcViewPointShape.point: TGISPoint;
   begin
      result.x := r_little_end_double (4);
      result.y := r_little_end_double (12)
   end;

procedure TArcViewPointShape.write_to_file (var f: file);
   begin
      assert (false, 'not implemented')
   end;


//  TArcViewPolyLineShape

constructor TArcViewPolyLineShape.CreateFromPolyLineShapeFile (var f: file; content_length: integer);
   var
      number_of_parts, total_number_of_points, part_no, p, points_in_part, pt_idx: integer;
   begin
      inherited CreateFromFile (f, content_length);
      number_of_parts := r_little_end_integer(36);
      total_number_of_points := r_little_end_integer(40);
      SetLength (Part, number_of_parts);
      SetLength (iPart, number_of_parts);
      pt_idx := 44 + (4*number_of_parts);
      for part_no := 0 to number_of_parts-1
      do begin
            if part_no < number_of_parts-1
            then  // not last part
               points_in_part := part_point_index(part_no+1) - part_point_index(part_no)
            else  // last part
               points_in_part := total_number_of_points - part_point_index(part_no);
            SetLength (Part[part_no], points_in_part);
            SetLength (iPart[part_no], points_in_part);
            for p := 0 to points_in_part-1
            do begin
                  Part[part_no, p].x := r_little_end_double (pt_idx);
                  Part[part_no, p].y := r_little_end_double (pt_idx + 8);
                  iPart[part_no, p].X := round (Part[part_no, p].x * iScale);
                  iPart[part_no, p].Y := round (Part[part_no, p].y * iScale);
                  pt_idx := pt_idx + 16
               end
         end
   end;

procedure TArcViewPolyLineShape.write_to_file (var f: file);
   var
      number_of_parts, total_number_of_points, part_no, p, points_in_part, pt_idx: integer;
   begin
      // this only handles same number of points as read in ...
      number_of_parts := r_little_end_integer(36);
      total_number_of_points := r_little_end_integer(40);
      pt_idx := 44 + (4*number_of_parts);
      for part_no := 0 to number_of_parts-1
      do begin
            if part_no < number_of_parts-1
            then  // not last part
               points_in_part := part_point_index(part_no+1) - part_point_index(part_no)
            else  // last part
               points_in_part := total_number_of_points - part_point_index(part_no);
            for p := 0 to points_in_part-1
            do begin
                  w_little_end_double (Part[part_no, p].x, pt_idx);
                  w_little_end_double (Part[part_no, p].y, pt_idx + 8);
                  pt_idx := pt_idx + 16
               end
         end;
      inherited
   end;

function TArcViewPolyLineShape.part_point_index (part_no: integer): integer;
   begin
      result := r_little_end_integer (44 + (4*part_no))
   end;

function TArcViewPolyLineShape.CreateCopy: TArcViewShapeBaseClass;
   var i: integer;
   begin
      result := TArcViewPolyLineShape.Create (size);
      for i := 0 to size-1 do
         result.b[i] := b[i]
   end;

function TArcViewPolyLineShape.shape_type: integer;
   begin
      result := polyline_shape_type
   end;

destructor TArcViewPolyLineShape.Destroy;
   var p: integer;
   begin
      for p := 0 to length(Part)-1
      do Part[p] := nil;
      Part := nil;
      inherited
   end;

function TArcViewPolyLineShape.number_of_parts: integer;
   begin
      result := r_little_end_integer (36)
   end;

{function TArcViewPolyLineShape.point (part, idx: integer): TGISPoint;
var
   part_idx: integer;
begin
   assert ((0 <= part) and (part <= number_of_parts-1));
   part_idx := r_little_end_integer (44 + (part*4));
   result.x := part_idx
end;
 }


//  TArcViewPolyLineZShape

constructor TArcViewPolyLineZShape.CreateFromPolyLineZShapeFile (var f: file; content_length: integer);
   begin
      inherited CreateFromPolyLineShapeFile (f, content_length);
   end;

function TArcViewPolyLineZShape.shape_type: integer;
   begin
      result := polylineZ_shape_type
   end;



//==============================================================================
//  TArcViewPolygonShape

constructor TArcViewPolygonShape.CreateEmpty;
   begin
      Create (36);    // alloc space for bounding box
      SetLength (Part, 1);
      SetLength (iPart, 1)
   end;

procedure TArcViewPolygonShape.Clear;
   begin
      SetLength(Part[0], 0);
      SetLength(iPart[0], 0)
   end;

procedure TArcViewPolygonShape.Add (pt: TGISPoint);
   begin
      Add (pt.x, pt.y)
   end;

procedure TArcViewPolygonShape.Add (x,y: real);
   var
      i: integer;
      pt: TGISPoint;
   begin
      i := Length(Part[0]);
      SetLength(Part[0], i+1);
      SetLength(iPart[0], i+1);
      Part[0,i].x := x;
      Part[0,i].y := y;
      iPart[0,i].x := Round (x * iScale);
      iPart[0,i].y := Round (y * iScale);
      pt.x := x;
      pt.y := y;
      if i = 0 then
         initialize_bounding_box (pt)
      else
         include_in_bounding_box (pt);

   end;

function TArcViewPolygonShape.CreateCopy: TArcViewShapeBaseClass;
   var i: integer;
   begin
      result := TArcViewPolygonShape.Create (size);
      for i := 0 to size-1 do
         result.b[i] := b[i]
   end;

function TArcViewPolygonShape.shape_type: integer;
   begin
      result := 5
   end;

function TArcViewPolygonShape.polygon_area (const polygon: TGISPointArray): real;
   var i,j: integer;
   begin
      result := 0;
      j := 0;
      for i := 0 to Length(polygon)-1
      do begin
            j := j + 1;
            if j = Length(polygon)
            then j := 0;
            result := result + ((polygon[i].x+polygon[j].x)*(polygon[i].y-polygon[j].y))
         end;
      result := result / 2
   end;

function TArcViewPolygonShape.area: real;  // in sq-GIS units
   var i: integer;
   begin
      result := 0;
      for i := 0 to Length(part)-1
      do result := result + polygon_area (part[i])
   end;

function TArcViewPolygonShape.contains (pt: TGISPoint): boolean;
   function in_polygon (polygon: TGISPointArray): boolean;
      function intersects (p1,p2: TGISPoint): boolean;
         function m: real;   // slope
            begin
               m := (p2.y-p1.y)/(p2.x-p1.x)
            end;
         function b: real;  // intercept
            begin
               b := p1.y - (m*p1.x)
            end;
         begin
            if ((p1.y < pt.y) and (p2.y < pt.y))
               or
               ((p1.y >= pt.y) and (p2.y >= pt.y))
            then
               result := false
            else
               if (p1.x = p2.x)
               then  // infinite slope
                  result := p1.x >= pt.x
               else
                  result := ((pt.y-b)/m) >= pt.x
         end;
      var
         i, intersection_count: integer;
      begin
         intersection_count := 0;
         for i := 0 to length(polygon)-2
         do if intersects (polygon[i], polygon[i+1])
            then
               intersection_count := intersection_count + 1;
         in_polygon := odd (intersection_count)
      end;
   var i, count: integer;
   begin
      result := false;  // provisional
      if (bounding_box_Xmin <= pt.x) and (pt.x <= bounding_box_Xmax)
         and
         (bounding_box_Ymin <= pt.y) and (pt.y <= bounding_box_Ymax)
      then
         begin
            count := 0;
            for i := 0 to Length(part)-1
            do if in_polygon (part[i])
               then
                  if polygon_area (part[i]) > 0
                  then
                     count := count + 1
                  else
                     count := count - 1;
            result := odd (count)
         end
   end;

function TArcViewPolygonShape.distance_from_boundary (pt: TGISPoint): real;
   const
      impossibly_large_distance = 1e308;  // largest 8-byte real is 1.7*10^308

   function distance_from_pt_to_line_segment (p0, p1, p2: TGISPoint): real;
      // p0 is the point; p1 & p2 are end points of line segment
      function DistancePointLine(const px, py, x1, y1, x2, y2: extended ): extended;
         //========================================================
         //  DistancePointLine Credits:
         //  Theory by Paul Bourke http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/
         //  Based in part on C code by Damian Coventry Tuesday, 16 July 2002
         //  Based on VBA code by Brandon Crosby 9-6-05 (2 dimensions)
         //  This Pascal (Delphi v7) implementation by Graham O'Brien 2007-10-13
         //
         //  px, py is the point to test.
         //  x1, y1, x2, y2 is the line to check distance.
         //
         //  Returns distance from the line, or if the intersecting point on the line nearest
         //    the point tested is outside the endpoints of the line, the distance to the
         //    nearest endpoint.
         //
         //  Returns impossibly_large_distance on zero-valued denominator conditions to return an illegal distance. (
         //    modification of Brandon Crosby's VBA code)
         //========================================================
         function SqLineMagnitude(const x1, y1, x2, y2: extended): extended;
            //  Returns the square of the magnitude of the line
            //    to cut down on unnecessary Sqrt when in many cases
            //    DistancePointLine() squares the result
            //
            begin
               result := (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1);
            end;
         function min (a,b: extended): extended;
            begin
               if a < b
               then result := a
               else result := b
            end;
         const
            EPS    = 0.000001;         // smallest positive value: less than that to be considered zero
            EPSEPS = EPS * EPS;        // and its square
         var
            SqLineMag,              // square of line's magnitude (see note in function LineMagnitude)
            u,                      // see Paul Bourke's original article(s)
            ix,                     // intersecting point X
            iy:         extended;   // intersecting point Y

         begin  // DistancePointLine
            SqLineMag := SqLineMagnitude(x1, y1, x2, y2);
            if SqLineMag < EPSEPS
            then
               begin
                  result := impossibly_large_distance;
                  exit;
               end;

            u := ( (px - x1)*(x2 - x1) + (py - y1)*(y2 - y1) ) / SqLineMag;

            if (u < EPS) or (u > 1)
            then
               begin
                  //  Closest point does not fall within the line segment,
                  //    take the shorter distance to an endpoint
                  ix := SqLineMagnitude(px, py, x1, y1);
                  iy := SqLineMagnitude(px, py, x2, y2);
                  result := min(ix, iy);
               end //  if (u < EPS) or (u > 1)
            else
               begin
                  //  Intersecting point is on the line, use the formula
                  ix := x1 + u * (x2 - x1);
                  iy := y1 + u * (y2 - y1);
                  result := SqlineMagnitude(px, py, ix, iy);
               end; //  else NOT (u < EPS) or (u > 1)

            // finally convert to actual distance not its square

            result := sqrt(result);
         end;  // DistancePointLine
      begin
         result := DistancePointLine(p0.x, p0.y, p1.x, p1.y, p2.x, p2.y);
      end;
   function min (a,b: real): real;
      begin
         if a < b
         then result := a
         else result := b
      end;
   var i, j: integer;
   begin
      result := impossibly_large_distance;
      for i := 0 to Length(Part)-1
      do begin
            result := min (result, distance_from_pt_to_line_segment (pt, Part[i][0], Part[i][Length(Part[i])-1]));
            for j := 0 to Length(Part[i])-2
            do result := min (result, distance_from_pt_to_line_segment (pt, Part[i][j], Part[i][j+1]))
         end;
      assert (result >= 0);  // DistancePointLine would have returned -1 if divide by 0 anywhere
      assert (result <> impossibly_large_distance);   // all points in polygon were identical??
      if contains (pt)
      then
         result := -result
   end;

function TArcViewPolygonShape.Intersects (poly: TArcViewPolygonShape): boolean;
   var
      clipper: TClipper;
      intersection: TPaths;
   begin
      clipper := TClipper.Create;
      clipper.AddPaths (iPart, ptSubject, true);
      clipper.AddPaths (poly.iPart, ptClip, true);
      clipper.Execute (ctIntersection, intersection, pftNonZero);
      result := Length(intersection) > 0;
      clipper.Free
   end;

function TArcViewPolygonShape.Intersection (poly: TArcViewPolygonShape): TArcViewPolygonShape;
   var
      clipper: TClipper;
      intersection: TPaths;
      i,j: integer;
   begin
      clipper := TClipper.Create;
      clipper.AddPaths (iPart, ptSubject, true);
      clipper.AddPaths (poly.iPart, ptClip, true);
      clipper.Execute (ctIntersection, intersection, pftNonZero);
      clipper.Free;

      result := TArcViewPolygonShape.Create (0);  // underlying byte array not used...
      SetLength (result.Part, Length(intersection));
      SetLength (result.iPart, Length(intersection));
      for i := 0 to Length(intersection)-1 do
         begin
            SetLength(result.Part[i], Length(intersection[i]));
            SetLength (result.iPart[i], Length(intersection[i]));
            for j := 0 to Length(intersection[i])-1 do
               begin
                  result.iPart[i,j].X := intersection[i,j].X;
                  result.iPart[i,j].Y := intersection[i,j].Y;
                  result.Part[i,j].X := intersection[i,j].X / iScale;
                  result.Part[i,j].Y := intersection[i,j].Y / iScale
               end
         end
   end;


//==============================================================================
// TArcViewShapeFile

constructor TArcViewShapeFile.Create (shape_type: integer);
   begin
      file_header := TArcViewFileHeader.Create (shape_type);
   end;

constructor TArcViewShapeFile.CreateFromFile (file_name: string);
   var
      i: integer;
      f: file;
      idx: TArcViewIndexFile;
      idx_file_name: string;
   begin
      // determine number of shapes from information in index file
      idx_file_name := file_name;
      idx_file_name[length(file_name)] := 'x';
      idx := TArcViewIndexFile.CreateFromFile (idx_file_name);
      number_of_shapes := idx.number_of_records;
      idx.Free;

      assignfile (f, file_name);
      reset (f, 1);
      file_header := TArcViewFileHeader.CreateFromFile (f);
      for i := 1 to number_of_shapes
      do begin
            f_shape_headers[i] := TArcViewShapeHeader.CreateFromFile (f);
            case f_shape_headers[i].shape_type of
               null_shape_type:
                  shapes[i] := TArcViewPointShape.CreateFromFile (f, f_shape_headers[i].content_length);
               point_shape_type:
                  shapes[i] := TArcViewPointShape.CreateFromFile (f, f_shape_headers[i].content_length);
               polyline_shape_type:
                  shapes[i] := TArcViewPolyLineShape.CreateFromPolyLineShapeFile (f, f_shape_headers[i].content_length);
               polygon_shape_type:
                  shapes[i] := TArcViewPolygonShape.CreateFromPolyLineShapeFile (f, f_shape_headers[i].content_length);
               polylineZ_shape_type:
                  shapes[i] := TArcViewPolyLineZShape.CreateFromPolyLineShapeFile (f, f_shape_headers[i].content_length);
            else
               assert (false, format ('unsupported shape %d', [f_shape_headers[i].shape_type]))
            end
         end;
      closefile (f)
   end;

destructor TArcViewShapeFile.Destroy;
   var i: integer;
   begin
      file_header.Free;
      for i := 1 to number_of_shapes
      do begin
            f_shape_headers[i].Free;
            shapes[i].Free
         end
   end;

procedure TArcViewShapeFile.Add (shape: TArcViewShapeBaseClass);
   begin
      f_number_of_shapes := f_number_of_shapes+1;
      SetLength (f_shape_headers, f_number_of_shapes+1);
      f_shape_headers[f_number_of_shapes] := TArcViewShapeHeader.Create (f_number_of_shapes, shape);
      SetLength(f_shapes, f_number_of_shapes+1);
      f_shapes[f_number_of_shapes] := shape.CreateCopy
   end;

procedure TArcViewShapeFile.WriteToFile (file_name: string);
   var
      i: integer;
      file_len: integer;
      f: file;
      idx: TArcViewIndexFile;
   begin
      assignfile (f, file_name);
      rewrite (f, 1);

      file_len := 50; // file header length in words
      for i := 1 to number_of_shapes do
         file_len := file_len + ((f_shape_headers[i].size + shapes[i].size) div 2);
      file_header.w_file_length (file_len);

      case file_header.r_shape_type of
         point_shape_type:
            begin
               file_header.initialize_bounding_box (TArcViewPointShape (shapes[1]).point);
               for i := 2 to number_of_shapes
               do file_header.include_in_bounding_box (TArcViewPointShape (shapes[i]).point)
            end;
         polyline_shape_type,
         polygon_shape_type,
         polylineZ_shape_type:
            begin
               file_header.initialize_bounding_box (TArcViewShapeWithBoundingBoxBaseClass (shapes[1]));
               for i := 2 to number_of_shapes
               do file_header.include_in_bounding_box (TArcViewShapeWithBoundingBoxBaseClass (shapes[i]))
            end;
      else
         assert (false)
      end;

      file_header.write_to_file (f);

      for i := 1 to number_of_shapes
      do begin
            f_shape_headers[i].write_to_file (f);
            shapes[i].write_to_file (f)
         end;

      closefile (f);

      idx := TArcViewIndexFile.CreateFromTArcViewShapeFile (Self);
      file_name[Length(file_name)] := 'x';   // change .shp to .shx
      idx.WriteToFile (file_name);
      idx.Free
   end;

procedure TArcViewShapeFile.w_number_of_shapes (val: integer);
   begin
      f_number_of_shapes := val;
      SetLength (f_shape_headers, val+1);
      SetLength (f_shapes, val+1);
   end;

function TArcViewShapeFile.r_shape (idx: integer): TArcViewShapeBaseClass;
   begin
      result := f_shapes[idx]
   end;

procedure TArcViewShapeFile.w_shape (idx: integer; val: TArcViewShapeBaseClass);
   begin
      assert ((0 < idx) and (idx <= number_of_shapes));
      assert (file_header.shape_type = val.shape_type);
      f_shapes[idx] := val
   end;

END.
