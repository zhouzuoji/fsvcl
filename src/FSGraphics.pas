unit FSGraphics;

interface

uses
  SysUtils, Classes, Consts, Windows, Graphics, FSVclBase;

type
  TFsDrawable = class(TComponent)
  private
    FUpdateCount: Integer;
    FOnChangeListeners: array of TExNotifyEvent;
  protected
    procedure DoChange(ID: TNotifyID);
    function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
    function GetEmpty: Boolean; virtual; abstract;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function HorzSafeStretch: Boolean; virtual;
    function VertSafeStretch: Boolean; virtual;
    procedure AddOnChangeListener(listener: TExNotifyEvent);
    procedure RemoveOnChangeListener(listener: TExNotifyEvent);
    procedure BeginUpdate;
    procedure EndUpdate;
    property Empty: Boolean read GetEmpty;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  TFsSingleDrawable = class(TFsDrawable)
  public
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
  end;

  TFsRectangle = class(TFsSingleDrawable)
  private
    FBorderColor: TColor;
    FColor: TColor;
    FBorderWidth: Integer;
    FBrushStyle: TBrushStyle;
    procedure SetBorderWidth(const Value: Integer);
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBrushStyle(const Value: TBrushStyle);
  protected
    function GetEmpty: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function HorzSafeStretch: Boolean; override;
    function VertSafeStretch: Boolean; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
  published
    property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle;
    property Color: TColor read FColor write SetColor;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
  end;

  TFsGradientDrawer = class(TFsSingleDrawable)
  private
    FTopLeftColor: TColor;
    FBottomRightColor: TColor;
    FVertical: Boolean;
    procedure SetBottomRightColor(const Value: TColor);
    procedure SetTopLeftColor(const Value: TColor);
    procedure SetVertical(const Value: Boolean);
  published
  public
    function HorzSafeStretch: Boolean; override;
    function VertSafeStretch: Boolean; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
  published
    property Vertical: Boolean read FVertical write SetVertical;
    property TopLeftColor: TColor read FTopLeftColor write SetTopLeftColor;
    property BottomRightColor: TColor read FBottomRightColor write SetBottomRightColor;
  end;

  TFsMultiFrameDrawable = class(TFsDrawable)
  public
    procedure DrawFrame(DC: HDC; const Rect: TRect; Index: Integer); virtual; abstract;
  end;

  TFsPictureDrawable = class(TFsSingleDrawable)
  private
    FPicture: TPicture;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
  protected
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetEmpty: Boolean; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
  published
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TFsNinePitchDrawable = class(TFsMultiFrameDrawable)
  private
    FGraphic: TBitmap;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    FMarginBottom: Integer;
    FRowCount: Integer;
    FColCount: Integer;
    procedure SetIntegerField(var field: Integer; value: Integer);
    procedure SetGraphic(const Value: TBitmap);
    procedure SetMarginBottom(const Value: Integer);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
    procedure PictureChanged(Sender: TObject);
    procedure SetTransparent(const Value: Boolean);
    procedure SetTransparentColor(const Value: TColor);
    function GetTransparent: Boolean;
    function GetTransparentColor: TColor;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetEmpty: Boolean; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function HorzSafeStretch: Boolean; override;
    function VertSafeStretch: Boolean; override;
    procedure DrawFrame(DC: HDC; const Rect: TRect; Index: Integer); override;
  published
    property Graphic: TBitmap read FGraphic write SetGraphic;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft;
    property MarginRight: Integer read FMarginRight write SetMarginRight;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
    property MarginBottom: Integer read FMarginBottom write SetMarginBottom;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property TransparentColor: TColor read GetTransparentColor write SetTransparentColor default clFuchsia;
    property RowCount: Integer read FRowCount write SetRowCount;
    property ColCount: Integer read FColCount write SetColCount;
  end;

procedure GradientFillRect(dc: THandle; const rect: TRect; TopLeftColor, BottomRightColor: TColor; IsVertical: Boolean);
procedure GradientFillTriangle(dc: THandle; x1, y1, x2, y2, x3, y3: Integer; color1, color2, color3: TColor);

procedure DrawBitmapSlice(DestDC: HDC; DestLeft, DestTop, DestWidth, DestHeight: Integer;
  bmp: TBitmap; SrcLeft, SrcTop, SrcWidth, SrcHeight: Integer);

implementation

type
  TBitmapHack = class(TBitmap);
  TCanvasHack = class(TCanvas);

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

procedure DrawBitmapSlice(DestDC: HDC; DestLeft, DestTop, DestWidth, DestHeight: Integer;
  bmp: TBitmap; SrcLeft, SrcTop, SrcWidth, SrcHeight: Integer);
var
  OldPalette: HPALETTE;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
  SrcDC, MaskDC: HDC;
  Save: THandle;
begin
  //ACanvas.RequiredState(csAllValid);

  TBitmapHack(bmp).PaletteNeeded;
  OldPalette := 0;
  RestorePalette := False;

  if bmp.Palette <> 0 then
  begin
    OldPalette := SelectPalette(DestDC, bmp.Palette, True);
    RealizePalette(DestDC);
    RestorePalette := True;
  end;

  { Call MaskHandleNeeded prior to creating the canvas handle since
    it causes FreeContext to be called. }

  if bmp.Transparent then TBitmapHack(bmp).MaskHandleNeeded;

  //TCanvasHack(bmp.Canvas).RequiredState(csAllValid);
    
  SrcDC := bmp.Canvas.Handle;
    
  BPP := GetDeviceCaps(DestDC, BITSPIXEL) * GetDeviceCaps(DestDC, PLANES);
  DoHalftone := (BPP <= 8) and (BPP < (GetDeviceCaps(SrcDC, BITSPIXEL) * GetDeviceCaps(SrcDC, PLANES)));

  if DoHalftone then
  begin
    GetBrushOrgEx(DestDC, pt);
    SetStretchBltMode(DestDC, HALFTONE);
    SetBrushOrgEx(DestDC, pt.x, pt.y, @pt);
  end
  else if not bmp.Monochrome then
    SetStretchBltMode(DestDC, STRETCH_DELETESCANS);

  try
    if bmp.Transparent then
    begin
      Save := 0;
      MaskDC := 0;

      try
        MaskDC := GDICheck(CreateCompatibleDC(0));
        Save := SelectObject(MaskDC, bmp.MaskHandle);

        TransparentStretchBlt(DestDC, DestLeft, DestTop, DestWidth, DestHeight,
          SrcDC, SrcLeft, SrcTop, SrcWidth, SrcHeight, MaskDC, SrcLeft, SrcTop);
      finally
        if Save <> 0 then SelectObject(MaskDC, Save);
        if MaskDC <> 0 then DeleteDC(MaskDC);
      end;
    end
    else
      StretchBlt(DestDC, DestLeft, DestTop, DestWidth, DestHeight,
        SrcDC, SrcLeft, SrcTop, SrcWidth, SrcHeight, Windows.SRCCOPY);
  finally
    if RestorePalette then
      SelectPalette(DestDC, OldPalette, True);
  end;
end;

procedure GradientFillRect(dc: THandle; const rect: TRect; TopLeftColor, BottomRightColor: TColor; IsVertical: Boolean);
var
  vertexs: array [0..1] of TRIVERTEX;
  color1, color2, mode: DWORD;
  gr: TGradientRect;
begin
  color1 := ColorToRGB(TopLeftColor);
  vertexs[0].x := rect.Left;
  vertexs[0].y := rect.Top;
  vertexs[0].Red := GetRValue(color1) * $ff00 div 255;
  vertexs[0].Green := GetGValue(color1) * $ff00 div 255;
  vertexs[0].Blue := GetBValue(color1) * $ff00 div 255;
  vertexs[0].Alpha := 0;

  color2 := ColorToRGB(BottomRightColor);
  vertexs[1].x := rect.Right;
  vertexs[1].y := rect.Bottom;
  vertexs[1].Red := GetRValue(color2) * $ff00 div 255;
  vertexs[1].Green := GetGValue(color2) * $ff00 div 255;
  vertexs[1].Blue := GetBValue(color2)* $ff00 div 255;
  vertexs[1].Alpha := 0;

  gr.UpperLeft := 0;
  gr.LowerRight := 1;

  if IsVertical then mode := GRADIENT_FILL_RECT_V
  else mode := GRADIENT_FILL_RECT_H;
  
  Windows.GradientFill(dc, @vertexs, 2, @gr, 1, mode);
end;

procedure GradientFillTriangle(dc: THandle; x1, y1, x2, y2, x3, y3: Integer; color1, color2, color3: TColor);
var
  vertexs: array [0..2] of TRIVERTEX;
  _color1, _color2, _color3: DWORD;
  gt: TGradientTriangle;
begin
  _color1 := ColorToRGB(color1);
  vertexs[0].x := x1;
  vertexs[0].y := y1;
  vertexs[0].Red := GetRValue(_color1) * $ff00 div 255;
  vertexs[0].Green := GetGValue(_color1) * $ff00 div 255;
  vertexs[0].Blue := GetBValue(_color1) * $ff00 div 255;
  vertexs[0].Alpha := 0;

  _color2 := ColorToRGB(color2);
  vertexs[1].x := x2;
  vertexs[1].y := y2;
  vertexs[1].Red := GetRValue(_color2) * $ff00 div 255;
  vertexs[1].Green := GetGValue(_color2) * $ff00 div 255;
  vertexs[1].Blue := GetBValue(_color2) * $ff00 div 255;
  vertexs[1].Alpha := 0;

  _color3 := ColorToRGB(color3);
  vertexs[2].x := x3;
  vertexs[2].y := y3;
  vertexs[2].Red := GetRValue(_color3) * $ff00 div 255;
  vertexs[2].Green := GetGValue(_color3) * $ff00 div 255;
  vertexs[2].Blue := GetBValue(_color3) * $ff00 div 255;
  vertexs[2].Alpha := 0;

  gt.Vertex1 := 0;
  gt.Vertex2 := 1;
  gt.Vertex3 := 2;

  Windows.GradientFill(dc, @vertexs, 3, @gt, 1, GRADIENT_FILL_TRIANGLE);
end;

{ TFsNinePitchDrawable }

procedure TFsNinePitchDrawable.Assign(Source: TPersistent);
begin
  if not Assigned(Source) then inherited
  else if Source is TGraphic then
  begin
    FGraphic.Assign(Source);
  end
  else if Source is TFsNinePitchDrawable then
  begin
    FMarginLeft := TFsNinePitchDrawable(Source).MarginLeft;
    FMarginRight := TFsNinePitchDrawable(Source).MarginRight;
    FMarginTop := TFsNinePitchDrawable(Source).MarginTop;
    FMarginBottom := TFsNinePitchDrawable(Source).MarginBottom;
    FGraphic.Assign(TFsNinePitchDrawable(Source).FGraphic);
    DoChange(niChange);
  end
  else inherited;
end;

procedure TFsNinePitchDrawable.AssignTo(Dest: TPersistent);
begin
  if not Assigned(Dest) then inherited;
  if Dest is TGraphic then Dest.Assign(Self)
  else inherited;
end;

constructor TFsNinePitchDrawable.Create(Owner: TComponent);
begin
  inherited;
  FGraphic := TBitmap.Create;
  FRowCount := 1;
  FColCount := 1;
  Self.Transparent := True;
  Self.TransparentColor := clFuchsia;
  FGraphic.OnChange := Self.PictureChanged;
end;

destructor TFsNinePitchDrawable.Destroy;
begin
  FGraphic.Free;
  inherited;
end;

procedure TFsNinePitchDrawable.DrawFrame(DC: HDC; const Rect: TRect; Index: Integer);
var
  SliceLeft, SliceRight, SliceTop, SliceBottom, FrameWidth, FrameHeight, RowIndex, ColIndex: Integer;
begin
  RowIndex := Index div FColCount;
  ColIndex := Index mod FColCount;

  FrameWidth := FGraphic.Width div FColCount;
  FrameHeight := FGraphic.Height div FRowCount;

  SliceLeft := ColIndex * FrameWidth;
  SliceRight := SliceLeft + FrameWidth;
  SliceTop := RowIndex * FrameHeight;
  SliceBottom := SliceTop + FrameHeight;

  // 左上
  DrawBitmapSlice(DC, Rect.Left, Rect.Top, FMarginLeft, FMarginTop,
    FGraphic, SliceLeft, SliceTop, FMarginLeft, FMarginTop);

  // 左中 
  DrawBitmapSlice(DC, Rect.Left, Rect.Top + FMarginTop, FMarginLeft, Rect.Bottom - FMarginBottom - Rect.Top - FMarginTop,
    FGraphic, SliceLeft, SliceTop + FMarginTop, FMarginLeft, SliceBottom - FMarginTop - SliceTop - FMarginTop);

  // 左下
  DrawBitmapSlice(DC, Rect.Left, Rect.Bottom - FMarginBottom, FMarginLeft, FMarginBottom, 
    FGraphic, SliceLeft, SliceBottom - FMarginBottom, FMarginLeft, FMarginBottom);

  // 右上
  DrawBitmapSlice(DC, Rect.Right - FMarginRight, Rect.Top, FMarginRight, FMarginTop, 
    FGraphic, SliceRight - FMarginRight, SliceTop, FMarginRight, FMarginTop);

  // 右中
  DrawBitmapSlice(DC, Rect.Right - FMarginRight, Rect.Top + FMarginTop, FMarginRight, Rect.Bottom - FMarginBottom - Rect.Top - FMarginTop, 
    FGraphic, SliceRight - FMarginRight, SliceTop + FMarginTop, FMarginRight, SliceBottom - FMarginBottom - SliceTop - FMarginTop);

  // 右下
  DrawBitmapSlice(DC, Rect.Right - FMarginRight, Rect.Bottom - FMarginBottom, FMarginRight, FMarginBottom, 
    FGraphic, SliceRight - FMarginRight, SliceBottom - FMarginBottom, FMarginRight, FMarginBottom);

  // 上中
  DrawBitmapSlice(DC, Rect.Left + FMarginLeft, Rect.Top, Rect.Right - FMarginRight - Rect.Left - FMarginLeft, FMarginTop, 
    FGraphic, SliceLeft + FMarginLeft, SliceTop, SliceRight - FMarginRight - SliceLeft - FMarginLeft, FMarginTop);

  // 下中
  DrawBitmapSlice(DC, Rect.Left + FMarginLeft, Rect.Bottom - FMarginBottom, Rect.Right - FMarginRight - Rect.Left - FMarginLeft, FMarginBottom, 
    FGraphic, SliceLeft + FMarginLeft, SliceBottom - FMarginBottom, SliceRight - FMarginRight - SliceLeft - FMarginLeft, FMarginBottom);

  // 中央
  DrawBitmapSlice(DC, Rect.Left + FMarginLeft, Rect.Top + FMarginTop, Rect.Right - FMarginRight - Rect.Left - FMarginLeft, Rect.Bottom - FMarginBottom - Rect.Top - FMarginTop, 
    FGraphic, SliceLeft + FMarginLeft, SliceTop + FMarginTop, SliceRight - FMarginRight - SliceLeft - FMarginLeft, SliceBottom - FMarginBottom - SliceTop - FMarginTop);
end;

function TFsNinePitchDrawable.GetEmpty: Boolean;
begin
  Result := FGraphic.Empty;
end;

function TFsNinePitchDrawable.GetHeight: Integer;
begin
  Result := FGraphic.Height div FRowCount;
end;

function TFsNinePitchDrawable.GetTransparent: Boolean;
begin
  Result := FGraphic.Transparent;
end;

function TFsNinePitchDrawable.GetTransparentColor: TColor;
begin
  Result := FGraphic.TransparentColor;
end;

function TFsNinePitchDrawable.GetWidth: Integer;
begin
  Result := FGraphic.Width div FColCount;
end;

procedure TFsNinePitchDrawable.PictureChanged(Sender: TObject);
begin
  DoChange(niChange);
end;

procedure TFsNinePitchDrawable.SetColCount(const Value: Integer);
begin
  if (FColCount <> Value) and (Value >= 1) then
  begin
    FColCount := Value;
    DoChange(niChange);
  end;
end;

procedure TFsNinePitchDrawable.SetGraphic(const Value: TBitmap);
begin
  if FGraphic <> Value then
  begin
    FGraphic.Assign(Value);
    DoChange(niChange);
  end;
end;

procedure TFsNinePitchDrawable.SetIntegerField(var field: Integer;
  value: Integer);
begin
  if field <> value then
  begin
    field := value;
    DoChange(niChange);
  end;
end;

procedure TFsNinePitchDrawable.SetMarginBottom(const Value: Integer);
begin
  SetIntegerField(FMarginBottom, Value);
end;

procedure TFsNinePitchDrawable.SetMarginLeft(const Value: Integer);
begin
  SetIntegerField(FMarginLeft, Value);
end;

procedure TFsNinePitchDrawable.SetMarginRight(const Value: Integer);
begin
  SetIntegerField(FMarginRight, Value);
end;

procedure TFsNinePitchDrawable.SetMarginTop(const Value: Integer);
begin
  SetIntegerField(FMarginTop, Value);
end;

procedure TFsNinePitchDrawable.SetRowCount(const Value: Integer);
begin
  if (FRowCount <> Value) and (Value >= 1) then
  begin
    FRowCount := Value;
    DoChange(niChange);
  end;
end;

procedure TFsNinePitchDrawable.SetTransparent(const Value: Boolean);
begin
  if FGraphic.Transparent <> Value then
  begin
    FGraphic.Transparent := Value;
    DoChange(niChange);
  end;
end;

procedure TFsNinePitchDrawable.SetTransparentColor(const Value: TColor);
begin
  if FGraphic.TransparentColor <> Value then
  begin
    FGraphic.TransparentColor := Value;
    DoChange(niChange);
  end;
end;

function TFsNinePitchDrawable.VertSafeStretch: Boolean;
begin
  Result := True;
end;

function TFsNinePitchDrawable.HorzSafeStretch: Boolean;
begin
  Result := True;
end;

{ TFsDrawable }

procedure TFsDrawable.AddOnChangeListener(listener: TExNotifyEvent);
var
  i: Integer;
begin
  if csDestroying in ComponentState then
    raise Exception.Create('Referring destroying component.');
  for i := Low(FOnChangeListeners) to High(FOnChangeListeners) do
  begin
    if ((TMethod(FOnChangeListeners[i])).Data = TMethod(listener).Data) and
      ((TMethod(FOnChangeListeners[i])).Code = TMethod(listener).Code) then Exit;
  end;
  SetLength(FOnChangeListeners, Length(FOnChangeListeners) + 1);
  FOnChangeListeners[High(FOnChangeListeners)] := listener;
end;

procedure TFsDrawable.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TFsDrawable.Create(Owner: TComponent);
begin
  inherited;

end;

destructor TFsDrawable.Destroy;
begin
  DoChange(niDestroy);
  SetLength(FOnChangeListeners, 0);
  inherited;
end;

procedure TFsDrawable.DoChange(ID: TNotifyID);
var
  i: Integer;
begin
  if (FUpdateCount = 0) or (ID = niDestroy) then
  begin
    i := High(FOnChangeListeners);
    while i >= Low(FOnChangeListeners) do
    begin
      FOnChangeListeners[i](Self, ID);
      Dec(i);
      if i > High(FOnChangeListeners) then
        i := High(FOnChangeListeners);
    end;
  end;
end;

procedure TFsDrawable.EndUpdate;
begin
  Dec(FUpdateCount);

  if FUpdateCount = 0 then DoChange(niChange);
end;

function TFsDrawable.GetHeight: Integer;
begin
  Result := -1;
end;

function TFsDrawable.GetWidth: Integer;
begin
  Result := -1;
end;

procedure TFsDrawable.RemoveOnChangeListener(listener: TExNotifyEvent);
var
  i, j: Integer;
begin
  for i := Low(FOnChangeListeners) to High(FOnChangeListeners) do
  begin
    if ((TMethod(FOnChangeListeners[i])).Data = TMethod(listener).Data) and
      ((TMethod(FOnChangeListeners[i])).Code = TMethod(listener).Code) then
    begin
      for j := i to High(FOnChangeListeners) - 1 do
        FOnChangeListeners[i] := FOnChangeListeners[i + 1];
      SetLength(FOnChangeListeners, Length(FOnChangeListeners) - 1);
      Break;
    end;
  end;
end;

function TFsDrawable.VertSafeStretch: Boolean;
begin
  Result := False;
end;

function TFsDrawable.HorzSafeStretch: Boolean;
begin
  Result := False;
end;

{ TFsPictureDrawable }

constructor TFsPictureDrawable.Create(Owner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FPicture.OnChange := Self.PictureChanged;
end;

destructor TFsPictureDrawable.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TFsPictureDrawable.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  inherited;

  if Assigned(FPicture.Graphic) then
    ACanvas.StretchDraw(Rect, FPicture.Graphic);
end;

function TFsPictureDrawable.GetEmpty: Boolean;
begin
  Result := not Assigned(FPicture.Graphic) or FPicture.Graphic.Empty;
end;

function TFsPictureDrawable.GetHeight: Integer;
begin
  Result := Picture.Height;
end;

function TFsPictureDrawable.GetWidth: Integer;
begin
  Result := Picture.Width;
end;

procedure TFsPictureDrawable.PictureChanged(Sender: TObject);
begin
  DoChange(niChange);
end;

procedure TFsPictureDrawable.SetPicture(const Value: TPicture);
begin
  if FPicture <> Value then
    FPicture.Assign(Value);
end;

{ TFsRectangle }

constructor TFsRectangle.Create(AOwner: TComponent);
begin
  inherited;
  FBorderColor := clBlack;
  FBorderWidth := 1;
  FBrushStyle := bsSolid;
  FColor := clBtnFace;
end;

procedure TFsRectangle.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  ACanvas.Brush.Style := Self.FBrushStyle;
  ACanvas.Brush.Color := Self.FColor;
  ACanvas.Pen.Width := Self.FBorderWidth;
  ACanvas.Pen.Color := Self.FBorderColor;
  ACanvas.Rectangle(Rect);
end;

function TFsRectangle.GetEmpty: Boolean;
begin
  Result := False;
end;

function TFsRectangle.HorzSafeStretch: Boolean;
begin
  Result := True;
end;

procedure TFsRectangle.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    DoChange(niChange);
  end;
end;

procedure TFsRectangle.SetBorderWidth(const Value: Integer);
begin
  if (FBorderWidth <> Value) and (Value >= 0) then
  begin
    FBorderWidth := Value;
    DoChange(niChange);
  end;
end;

procedure TFsRectangle.SetBrushStyle(const Value: TBrushStyle);
begin
  if FBrushStyle <> Value then
  begin
    FBrushStyle := Value;
    DoChange(niChange);
  end;
end;

procedure TFsRectangle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange(niChange);
  end;
end;

function TFsRectangle.VertSafeStretch: Boolean;
begin
  Result := True;
end;

{ TFsGradientDrawer }

procedure TFsGradientDrawer.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  GradientFillRect(ACanvas.Handle, Rect, FTopLeftColor, FBottomRightColor, FVertical);
end;

function TFsGradientDrawer.HorzSafeStretch: Boolean;
begin
  Result := True;
end;

procedure TFsGradientDrawer.SetBottomRightColor(const Value: TColor);
begin
  if FBottomRightColor <> Value then
  begin
    FBottomRightColor := Value;
    DoChange(niChange);
  end;
end;

procedure TFsGradientDrawer.SetTopLeftColor(const Value: TColor);
begin
  if FTopLeftColor <> Value then
  begin
    FTopLeftColor := Value;
    DoChange(niChange);
  end;
end;

procedure TFsGradientDrawer.SetVertical(const Value: Boolean);
begin
  if FVertical <> Value then
  begin
    FVertical := Value;
    DoChange(niChange);
  end;
end;

function TFsGradientDrawer.VertSafeStretch: Boolean;
begin
  Result := True;
end;

end.

