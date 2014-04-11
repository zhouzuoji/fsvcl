unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FSStdCtrls, jpeg, ExtCtrls, FSGraphics, pngimage, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    FsButton1: TFsImageButton;
    FsButton2: TFsImageButton;
    FsButton3: TFsImageButton;
    FsButton4: TFsImageButton;
    FsButton5: TFsImageButton;
    FsButton6: TFsImageButton;
    FsButton7: TFsImageButton;
    FsButton8: TFsImageButton;
    FsCoverButton1: TFsCoverButton;
    FsPictureDrawable1: TFsPictureDrawable;
    Timer1: TTimer;
    Image1: TImage;
    Edit1: TEdit;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure FsButton7Click(Sender: TObject);
  private
    hdcMem: HDC;
    bmpMem: HBITMAP;
    procedure UpdateView;
    procedure WMEraseBkgnd(var msgr: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var msgr: TWMNCHitTest); message WM_NCHITTEST;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  dwExStyle: DWORD;
begin
	dwExStyle := GetWindowLong(Self.Handle, GWL_EXSTYLE or WS_EX_TRANSPARENT);
	SetWindowLong(Self.Handle, GWL_EXSTYLE, dwExStyle or WS_EX_LAYERED);

  hdcMem := CreateCompatibleDC(0);
  bmpMem := CreateCompatibleBitmap(0, Self.Width, Self.Height);
  SelectObject(hdcMem, bmpMem);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteDC(hdcMem);
end;

procedure TForm1.FsButton7Click(Sender: TObject);
begin
  ShowMessage(TFsImageButton(Sender).Caption);
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  ShowMessage('fuck');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Self.UpdateView;
  //Timer1.Enabled := False;
end;

procedure TForm1.UpdateView;
var
  rc: TRect;
  ptSrc, ptDest, pt: TPoint;
  sizeDest: TSize;
  dc, hMemDC: HDC;
  bmp: TBitmap;
  hOldBitmap: HBITMAP;
  blf: TBlendFunction;
  pline: PIntegerArray;
  i, j: Integer;
begin
  GetWindowRect(Self.Handle, rc);
  ptDest.X := rc.Left;
  ptDest.Y := rc.Top;
  sizeDest.cx := rc.Right - rc.Left;
  sizeDest.cy := rc.Bottom - rc.Top;
  dc := Windows.GetDC(Self.Handle);
  hMemDc := Windows.CreateCompatibleDC(dc);
  bmp := TBitmap.Create;
  bmp.SetSize(sizeDest.cx, sizeDest.cy);
  bmp.PixelFormat := pf32bit;
  //bmp.AlphaFormat := afPremultiplied;

  for i := 0 to bmp.Height - 1 do
  begin
    pline := PIntegerArray(bmp.ScanLine[i]);
    for j := 0 to bmp.Width - 1 do
      pline[j] := $000000;
  end;

  hOldBitmap := SelectObject(hMemDc, bmp.Handle);
  blf.BlendOp := AC_SRC_OVER;
  blf.BlendFlags := 0;
  blf.SourceConstantAlpha := 255;
  blf.AlphaFormat := AC_SRC_ALPHA;
  ptSrc.X := 0;
  ptSrc.Y := 0;


  pt := Self.ScreenToClient(ptDest);
  Self.PaintTo(hMemDC, -pt.X, -pt.Y);

  if not UpdateLayeredWindow(Self.Handle, 0, @ptDest,@sizeDest, hMemDc, @ptSrc, 0, @blf, ULW_ALPHA) then
  begin
    OutputDebugString(PChar(IntToStr(GetLastError)));
  end;

  SelectObject(hMemDc,hOldBitmap);
  bmp.Free;
  DeleteDC(hMemDc);
  ReleaseDC(Self.Handle, dc);
end;

procedure TForm1.WMEraseBkgnd(var msgr: TWMEraseBkgnd);
begin
  SetBkMode(msgr.DC, TRANSPARENT);
  msgr.Result := 1;
end;

procedure TForm1.WMNCHitTest(var msgr: TWMNCHitTest);
begin
  msgr.Result := HTCAPTION;
end;

end.
