object Form1: TForm1
  Left = 549
  Top = 309
  Caption = #33258#32472#26631#39064#26639#21644#28378#21160#26465
  ClientHeight = 557
  ClientWidth = 874
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    874
    557)
  PixelsPerInch = 96
  TextHeight = 17
  object FsImage1: TFsImage
    Left = 168
    Top = 160
    Width = 4
    Height = 100
    Picture = npdRightBorder
    AutoSize = True
    Visible = False
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 858
    Height = 541
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Memo'
      DesignSize = (
        850
        509)
      object FsMemo1: TFsMemo
        Left = 5
        Top = 4
        Width = 839
        Height = 493
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvNone
        BevelOuter = bvRaised
        BevelKind = bkFlat
        ParentBackground = False
        ScrollBarDrawer = FsFlatScrollBar1
        ScrollBars = ssBoth
        Lines.Strings = (
          'unit FSStdCtrls;'
          ''
          'interface'
          ''
          'uses'
          '  SysUtils, Classes, Consts, Windows, '
          'Graphics, Controls, Messages, StdCtrls, '
          'ExtCtrls,'
          '  FSVclBase, FSGraphics;'
          ''
          'type'
          '  TFsGraphicControl = class'
          '(TGraphicControl)'
          '  protected'
          '    procedure GetContentDimension(out dim: '
          'TSize); virtual;'
          '    function CanAutoSize(var NewWidth, '
          'NewHeight: Integer): Boolean; override;'
          '  public'
          '    function CheckAutoSize: Boolean;'
          '    procedure AutoSizeAndInvalidate;'
          '  end;'
          ''
          '  TFsImage = class(TFsGraphicControl)'
          '  private'
          '    FPicture: TFsDrawable;'
          '    procedure PictureChanged(Sender: '
          'TObject; ID: TNotifyID);'
          '    procedure SetPicture(const Value: '
          'TFsDrawable);'
          '  protected'
          '    procedure GetContentDimension(out dim: '
          'TSize); override;'
          '  public'
          '    constructor Create(Owner: '
          'TComponent); override;'
          '    destructor Destroy; override;'
          '    procedure Paint; override;'
          '  published'
          '    property Picture: TFsDrawable read '
          'FPicture write SetPicture;'
          '    property Action;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSize;'
          '    property Enabled;'
          '    property ShowHint;'
          '    property Visible;'
          '    property OnClick;'
          '  end;'
          ''
          '  TFsImageButtonLayout = (blPictureLeft, '
          'blPictureRight, blPictureTop, '
          'blPictureBottom, blTextInPicture, '
          'blTextInStretchPicture);'
          ''
          '  TFsCustomButton = class'
          '(TFsGraphicControl)'
          '  private'
          '    FMouseFlag: TMouseFlag;'
          '    FShowCaption: Boolean;'
          '    FLayout: TFsImageButtonLayout;'
          '    FSpace: Integer;'
          '    procedure SetShowCaption(const Value: '
          'Boolean);'
          '    procedure SetLayout(const Value: '
          'TFsImageButtonLayout);'
          '    procedure SetSpace(const Value: '
          'Integer);'
          '    procedure DrawPictureLeft;'
          '    procedure DrawPictureRight;'
          '    procedure DrawPictureTop;'
          '    procedure DrawPictureBottom;'
          '    procedure DrawTextInPicture;'
          '    procedure DrawTextInStretchPicture;'
          '    procedure DrawButtonSurface;'
          '  protected'
          '    procedure DrawImageAndText;'
          '    procedure GetContentDimension(out dim: '
          'TSize); override;'
          '    procedure MouseDown(Button: '
          'TMouseButton; Shift: TShiftState; X, Y: '
          'Integer); override;'
          '    procedure MouseUp(Button: '
          'TMouseButton; Shift: TShiftState; X, Y: '
          'Integer); override;'
          '    procedure CMMouseEnter(var msg: '
          'TMessage); message CM_MOUSEENTER;'
          '    procedure CMMouseLeave(var msg: '
          'TMessage); message CM_MOUSELEAVE;'
          '    procedure CMTextChange(var msg: '
          'TMessage); message CM_TEXTCHANGED;'
          '    procedure CMFontChanged(var msgr: '
          'TMessage); message CM_FONTCHANGED;'
          '    function PictureIsHorzStretchable: '
          'Boolean; virtual;'
          '    function PictureIsVertStretchable: '
          'Boolean; virtual;'
          '    function GetPictureSize(out width: '
          'Integer): Integer; virtual; abstract;'
          '    procedure DrawPicture(const Rect: '
          'TRect); virtual; abstract;'
          '    procedure Paint; override;'
          '  public'
          '    constructor Create(Owner: '
          'TComponent); override;'
          '  published'
          '    property Layout: TFsImageButtonLayout '
          'read FLayout write SetLayout default '
          'blTextInStretchPicture;'
          '    property Space: Integer read FSpace '
          'write SetSpace default 4;'
          '    property ShowCaption: Boolean read '
          'FShowCaption write SetShowCaption default '
          'True;'
          '    property Action;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSize;'
          '    property Caption;'
          '    property Enabled;'
          '    property Font;'
          '    property ShowHint;'
          '    property Visible;'
          '    property OnClick;'
          '  end;'
          ''
          '  TFsImageButton = class'
          '(TFsCustomButton)'
          '  private'
          '    FPicture: TFsPictureDrawable;'
          '    FDisablePicture: TFsPictureDrawable;'
          '    FMouseOverPicture: TFsPictureDrawable;'
          '    FMouseDownPicture: TFsPictureDrawable;'
          '    procedure PictureChanged(Sender: '
          'TObject; ID: TNotifyID);'
          '    procedure SetPicture(const Value: '
          'TPicture);'
          '    procedure SetMouseDownPicture(const '
          'Value: TPicture);'
          '    procedure SetMouseOverPicture(const '
          'Value: TPicture);'
          '    procedure SetDisablePicture(const Value: '
          'TPicture);'
          '    function GetDisablePicture: TPicture;'
          '    function GetMouseDownPicture: TPicture;'
          '    function GetMouseOverPicture: TPicture;'
          '    function GetPicture: TPicture;'
          '    function GetDrawable: TFsSingleDrawable;'
          '  protected'
          '    function GetPictureSize(out width: '
          'Integer): Integer; override;'
          '    procedure DrawPicture(const Rect: '
          'TRect); override;'
          '  public'
          '    constructor Create(Owner: '
          'TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          '    property Picture: TPicture read GetPicture '
          'write SetPicture;'
          '    property DisablePicture: TPicture read '
          'GetDisablePicture write SetDisablePicture;'
          '    property MouseOverPicture: TPicture '
          'read GetMouseOverPicture write '
          'SetMouseOverPicture;'
          '    property MouseDownPicture: TPicture '
          'read GetMouseDownPicture write '
          'SetMouseDownPicture;'
          '  end;'
          ''
          '  TFsCoverButton = class'
          '(TFsCustomButton)'
          '  private'
          '    FPicture: TPicture;'
          '    FHoverCover: TFsDrawable;'
          '    fDownCover: TFsDrawable;'
          '    procedure SetPicture(const Value: '
          'TPicture);'
          '    procedure SetDownCover(const Value: '
          'TFsDrawable);'
          '    procedure SetHoverCover(const Value: '
          'TFsDrawable);'
          '    function GetCover: TFsDrawable;'
          '  protected'
          '    procedure PictureChanged(Sender: '
          'TObject);'
          '    procedure LinkedPictureChanged(Sender: '
          'TObject; ID: TNotifyID);'
          '    function GetPictureSize(out width: '
          'Integer): Integer; override;'
          '    procedure DrawPicture(const Rect: '
          'TRect); override;'
          '    procedure Paint; override;'
          '  public'
          '    constructor Create(AOwner: '
          'TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          '    property Picture: TPicture read FPicture '
          'write SetPicture;'
          '    property HoverCover: TFsDrawable read '
          'FHoverCover write SetHoverCover;'
          '    property DownCover: TFsDrawable read '
          'fDownCover write SetDownCover;'
          '  end;'
          ''
          '  TFsSkinButton = class(TFsCustomButton)'
          '  '
          '  end;'
          ''
          '  TFsCustomScrollBar = class(TComponent)'
          '  private'
          '    FVScrollWidth: Integer;'
          '    FHScrollHeight: Integer;'
          '    FVArrowHeight: Integer;'
          '    FHArrowWidth: Integer;'
          '    FMinThumbLength: Integer;'
          '    procedure SetHScrollHeight(const Value: '
          'Integer);'
          '    procedure SetVScrollWidth(const Value: '
          'Integer);'
          '    procedure SetHArrowWidth(const Value: '
          'Integer);'
          '    procedure SetVArrowHeight(const Value: '
          'Integer);'
          '    procedure SetMinThumbLength(const '
          'Value: Integer);'
          '  protected'
          '    procedure Changed;'
          '  public'
          '    constructor Create(AOwner: '
          'TComponent); override;'
          '    procedure CalcVScroll(const rc: TRect; '
          'const si: TScrollInfo; var rcTopArrow, '
          'rcBottomArrow, rcThumb: TRect);'
          '    procedure CalcHScroll(const rc: TRect; '
          'const si: TScrollInfo; var rcLeftArrow, '
          'rcRightArrow, rcThumb: TRect);'
          '    function CalcVPos(const rc: TRect; const '
          'si: TScrollInfo; Y: Integer): Integer;'
          '    function CalcHPos(const rc: TRect; const '
          'si: TScrollInfo; Y: Integer): Integer;'
          '    procedure DrawVScroll(dc: HDC; const '
          'rc, rcTopArrow, rcBottomArrow, rcThumb: '
          'TRect); virtual; abstract;'
          '    procedure DrawHScroll(dc: HDC; const '
          'rc, rcLeftArrow, rcRightArrow, rcThumb: '
          'TRect); virtual; abstract;'
          '    procedure DrawIntersect(dc: HDC; const '
          'rc: TRect); virtual; abstract;'
          '    property HScrollHeight: Integer read '
          'FHScrollHeight write SetHScrollHeight;'
          '    property VScrollWidth: Integer read '
          'FVScrollWidth write SetVScrollWidth;'
          '    property VArrowHeight: Integer read '
          'FVArrowHeight write SetVArrowHeight;'
          '    property HHArrowWidth: Integer read '
          'FHArrowWidth write SetHArrowWidth;'
          '    property MinThumbLength: Integer read '
          'FMinThumbLength write '
          'SetMinThumbLength;'
          '  end;'
          ''
          '  TFsFlatScrollBar = class'
          '(TFsCustomScrollBar)'
          '  private'
          '    procedure DrawUpArrow(dc: HDC; const '
          'rc: TRect; h, bw: Integer);'
          '    procedure DrawDownArrow(dc: HDC; '
          'const rc: TRect; h, bw: Integer);'
          '    procedure DrawLeftArrow(dc: HDC; const '
          'rc: TRect; h, bw: Integer);'
          '    procedure DrawRightArrow(dc: HDC; '
          'const rc: TRect; h, bw: Integer);'
          '  public'
          '    procedure DrawVScroll(dc: HDC; const '
          'rc, rcTopArrow, rcBottomArrow, rcThumb: '
          'TRect); override;'
          '    procedure DrawHScroll(dc: HDC; const '
          'rc, rcLeftArrow, rcRightArrow, rcThumb: '
          'TRect); override;'
          '    procedure DrawIntersect(dc: HDC; const '
          'rc: TRect); override;'
          '  end;'
          ''
          '  TFsScrollInfo = record'
          '    ShowVScroll: Boolean;'
          '    ShowHScroll: Boolean;'
          '    VScroll: TRect;'
          '    TopArrow: TRect;'
          '    BottomArrow: TRect;'
          '    VThumb: TRect;'
          '    HScroll: TRect;'
          '    LeftArrow: TRect;'
          '    RightArrow: TRect;'
          '    HThumb: TRect;'
          '    Intersect: TRect;'
          '  end;'
          ''
          '  TScrollHitTest = (shtNoWhere, shtBorder, '
          'shtLeftArrow, shtRightArrow, '
          'shtHorzThumb, shtPageLeft, shtPageRight,'
          '    shtTopArrow, shtBottomArrow, '
          'shtVertThumb, shtPageUp, shtPageDown);'
          ''
          '  TFsScrollContainer = class'
          '(TCustomControl)'
          '  private'
          '    FTimer: TTimer;'
          '    FCaptureRegion: TScrollHitTest;'
          '    FCapturePoint: TPoint;'
          '    FScrollBarDrawer: TFsCustomScrollBar;'
          '    FRealControl: TControl;'
          '    procedure SetScrollBarDrawer(const '
          'Value: TFsCustomScrollBar);'
          '    function GetRealScrollBar: '
          'TFsCustomScrollBar;'
          '    function NeedScrollBar(out HScroll: '
          'Boolean): Boolean;'
          '    procedure GetScrollInfo(var fsi: '
          'TFsScrollInfo; var vsi, hsi: TScrollInfo);'
          '    procedure OnTimer(Sender: TObject);'
          '    function ControlMessage(msg: DWORD; '
          'wparam, lparam: Integer): Integer;'
          '  protected'
          '    FMouseInControl: Boolean;'
          '    procedure WMSize(var msgr: TWMSize); '
          'message WM_SIZE;'
          '    procedure CMMouseEnter(var msgr: '
          'TMessage); message CM_MOUSEENTER;'
          '    procedure CMMouseLeave(var msgr: '
          'TMessage); message CM_MOUSELEAVE;'
          '    procedure MouseDown(Button: '
          'TMouseButton; Shift: TShiftState; X, Y: '
          'Integer); override;'
          '    procedure MouseUp(Button: '
          'TMouseButton; Shift: TShiftState; X, Y: '
          'Integer); override;'
          '    procedure MouseMove(Shift: TShiftState; '
          'X, Y: Integer); override;'
          '    procedure CreateParams(var Params: '
          'TCreateParams); override;'
          '    procedure Paint; override;'
          ''
          '    function GetControlScrollInfo(var si: '
          'TScrollInfo; isVert: Boolean): Boolean; '
          'virtual;'
          '    function CreateRealControl: TControl; '
          'virtual; abstract;'
          '  public'
          '    constructor Create(AOwner: '
          'TComponent); override;'
          '    destructor Destroy; override;'
          '    procedure AdjustInnerControlBounds;'
          '    function HitTest(X, Y: Integer): '
          'TScrollHitTest; '
          '    property Canvas;'
          '    property RealControl: TControl read '
          'FRealControl;'
          '  published'
          '    property Align;'
          '    property Anchors;'
          '    property ScrollBarDrawer: '
          'TFsCustomScrollBar read FScrollBarDrawer '
          'write SetScrollBarDrawer;'
          '  end;'
          ''
          '  TFsMemo = class(TFsScrollContainer)'
          '  private'
          '    function GetScrollBars: TScrollStyle;'
          '    procedure SetScrollBars(const Value: '
          'TScrollStyle);'
          '    function GetLines: TStrings;'
          '    procedure SetLines(const Value: '
          'TStrings);'
          '  protected'
          '    function CreateRealControl: TControl; '
          'override;'
          '  published'
          '    property ScrollBars: TScrollStyle read '
          'GetScrollBars write SetScrollBars;'
          '    property Lines: TStrings read GetLines '
          'write SetLines;'
          '  end;'
          ''
          '  TFsEdit = class(TCustomEdit)'
          '  private'
          '    FMouseInBorderColor: TColor;'
          '    procedure SetMouseInBorderColor(const '
          'Value: TColor);'
          '  protected'
          '    FMouseIn: Boolean;'
          '    procedure CMMouseEnter(var msg: '
          'TMessage); message CM_MOUSEENTER;'
          '    procedure CMMouseLeave(var msg: '
          'TMessage); message CM_MOUSELEAVE;'
          '    procedure WMNCPAINT(var msg: '
          'TWMNCPaint); message WM_NCPAINT;'
          '  public'
          '    constructor Create(AOwner: '
          'TComponent); override;'
          '  published'
          '    property MouseInBorderColor: TColor '
          'read FMouseInBorderColor write '
          'SetMouseInBorderColor;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSelect;'
          '    property AutoSize;'
          '    property BevelEdges;'
          '    property BevelInner;'
          '    property BevelKind default bkNone;'
          '    property BevelOuter;'
          '    property BevelWidth;'
          '    property BiDiMode;'
          '    property BorderStyle;'
          '    property CharCase;'
          '    property Color;'
          '    property Constraints;'
          '    property Ctl3D;'
          '    property DoubleBuffered;'
          '    property DragCursor;'
          '    property DragKind;'
          '    property DragMode;'
          '    property Enabled;'
          '    property Font;'
          '    property HideSelection;'
          '    property ImeMode;'
          '    property ImeName;'
          '    property MaxLength;'
          '    property OEMConvert;'
          '    property ParentBiDiMode;'
          '    property ParentColor;'
          '    property ParentCtl3D;'
          '    property ParentFont;'
          '    property ParentShowHint;'
          '    property PasswordChar;'
          '    property PopupMenu;'
          '    property ReadOnly;'
          '    property ShowHint;'
          '    property TabOrder;'
          '    property TabStop;'
          '    property Text;'
          '    property Visible;'
          '    property OnChange;'
          '    property OnClick;'
          '    property OnContextPopup;'
          '    property OnDblClick;'
          '    property OnDragDrop;'
          '    property OnDragOver;'
          '    property OnEndDock;'
          '    property OnEndDrag;'
          '    property OnEnter;'
          '    property OnExit;'
          '    property OnKeyDown;'
          '    property OnKeyPress;'
          '    property OnKeyUp;'
          '    property OnMouseActivate;'
          '    property OnMouseDown;'
          '    property OnMouseEnter;'
          '    property OnMouseLeave;'
          '    property OnMouseMove;'
          '    property OnMouseUp;'
          '    property OnStartDock;'
          '    property OnStartDrag;'
          '  end;'
          ''
          '  TFsButtonEdit = class(TFsEdit)'
          '  private'
          '    FOnClickButton: TNotifyEvent;'
          '    FButtonPicture: TPicture;'
          '    procedure WriteButtonPicture(const '
          'Value: TPicture);'
          '  protected'
          '    FNCCanvas: TCanvas;'
          '    procedure WMNCPAINT(var msgr: '
          'TWMNCPaint); message WM_NCPAINT;'
          '    procedure WMNCLButtonDown(var '
          'msgr: TWMNCLButtonDown); message '
          'WM_NCLBUTTONDOWN;'
          '    procedure WMNCHitTest(var msgr: '
          'TWMNCHitTest); message WM_NCHITTEST;'
          '    procedure WMNCCalcSize(var msgr: '
          'TWMNCCalcSize); message '
          'WM_NCCALCSIZE;'
          '  public'
          '    constructor Create(AOwner: '
          'TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          '    property ButtonPicture: TPicture read '
          'FButtonPicture write WriteButtonPicture;'
          '    property OnClickButton: TNotifyEvent '
          'read FOnClickButton write FOnClickButton;'
          '  end;'
          ''
          '  TFsNCScrollMemo = class(TCustomMemo)'
          '  private'
          '    FMouseInBorderColor: TColor;'
          '    FMouseIn: Boolean;'
          '    FScrollBar: TFsCustomScrollBar;'
          '    procedure PaintNC;'
          '    procedure CMMouseEnter(var msg: '
          'TMessage); message CM_MOUSEENTER;'
          '    procedure CMMouseLeave(var msg: '
          'TMessage); message CM_MOUSELEAVE;'
          '    procedure WMNCPAINT(var msgr: '
          'TWMNCPaint); message WM_NCPAINT;'
          '    procedure WMNCCalcSize(var msgr: '
          'TWMNCCalcSize); message '
          'WM_NCCALCSIZE;'
          '    procedure WMNCHitTest(var msgr: '
          'TWMNCHitTest); message WM_NCHITTEST;'
          '    procedure WMNCLButtonDown(var '
          'msgr: TWMNCLButtonDown); message '
          'WM_NCLBUTTONDOWN;'
          '    procedure SetMouseInBorderColor(const '
          'Value: TColor);'
          '    procedure SetScrollBar(const Value: '
          'TFsCustomScrollBar);'
          '    function GetRealScrollBar: '
          'TFsCustomScrollBar;'
          '  protected'
          '    procedure GetScrollRect(var rcVScroll, '
          'rcHScroll, rcIntersect: TRect);'
          '    function GetVScrollRect(var rc: TRect): '
          'Boolean;'
          '    function GetHScrollRect(var rc: TRect): '
          'Boolean;'
          '    procedure WndProc(var msgr: '
          'TMessage); override;'
          '    procedure Notification(AComponent: '
          'TComponent; Operation: TOperation); '
          'override;'
          '  public'
          '    constructor Create(AOwner: '
          'TComponent); override;'
          '  published'
          '    property ScrollBar: TFsCustomScrollBar '
          'read FScrollBar write SetScrollBar;'
          '    property MouseInBorderColor: TColor '
          'read FMouseInBorderColor write '
          'SetMouseInBorderColor;'
          '    property Align;'
          '    property Alignment;'
          '    property Anchors;'
          '    property BevelEdges;'
          '    property BevelInner;'
          '    property BevelKind default bkNone;'
          '    property BevelOuter;'
          '    property BiDiMode;'
          '    property BorderStyle;'
          '    property Color;'
          '    property Constraints;'
          '    property Ctl3D;'
          '    property DragCursor;'
          '    property DragKind;'
          '    property DragMode;'
          '    property Enabled;'
          '    property Font;'
          '    property HideSelection;'
          '    property ImeMode;'
          '    property ImeName;'
          '    property Lines;'
          '    property MaxLength;'
          '    property OEMConvert;'
          '    property ParentBiDiMode;'
          '    property ParentColor;'
          '    property ParentCtl3D;'
          '    property ParentFont;'
          '    property ParentShowHint;'
          '    property PopupMenu;'
          '    property ReadOnly;'
          '    property ScrollBars;'
          '    property ShowHint;'
          '    property TabOrder;'
          '    property TabStop;'
          '    property Visible;'
          '    property WantReturns;'
          '    property WantTabs;'
          '    property WordWrap;'
          '    property OnChange;'
          '    property OnClick;'
          '    property OnContextPopup;'
          '    property OnDblClick;'
          '    property OnDragDrop;'
          '    property OnDragOver;'
          '    property OnEndDock;'
          '    property OnEndDrag;'
          '    property OnEnter;'
          '    property OnExit;'
          '    property OnKeyDown;'
          '    property OnKeyPress;'
          '    property OnKeyUp;'
          '    property OnMouseActivate;'
          '    property OnMouseDown;'
          '    property OnMouseEnter;'
          '    property OnMouseLeave;'
          '    property OnMouseMove;'
          '    property OnMouseUp;'
          '    property OnStartDock;'
          '    property OnStartDrag;'
          '  end;'
          ''
          '  TFsBorderlessMemo = class(TMemo)'
          '  protected'
          '    procedure WndProc(var msgr: '
          'TMessage); override;'
          '    procedure CreateParams(var params: '
          'TCreateParams); override;'
          '    procedure WMNCCalcSize(var msgr: '
          'TWMNCCalcSize); message '
          'WM_NCCALCSIZE;'
          '    procedure WMNCPaint(var msgr: '
          'TWMNCPaint); message WM_NCPAINT;'
          '  public'
          '    constructor Create(AOwner: '
          'TComponent); override;'
          '  end;'
          ''
          '  TFsCustomCheckBox = class'
          '(TFsGraphicControl)'
          '  private'
          '    FChecked: Boolean;'
          '    FSpace: Integer;'
          '    procedure SetChecked(const Value: '
          'Boolean);'
          '    procedure SetSpace(const Value: '
          'Integer);'
          '  protected'
          '    procedure CMTextChange(var msg: '
          'TMessage); message CM_TEXTCHANGED;'
          '    procedure CMFontChange(var msg: '
          'TMessage); message CM_FONTCHANGED;'
          '    procedure GetImageSize(out w, h: '
          'Integer); virtual;'
          '    procedure DrawMark(const Rect: TRect); '
          'virtual; abstract;'
          '    procedure Paint; override;'
          '    procedure MouseDown(Button: '
          'TMouseButton; Shift: TShiftState; X, Y: '
          'Integer); override;'
          '    procedure GetContentDimension(out dim: '
          'TSize); override;'
          '  public'
          '    constructor Create(Owner: '
          'TComponent); override;'
          '  published'
          '    property Action;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSize;'
          '    property Enabled;'
          '    property Font;'
          '    property Hint;'
          '    property ShowHint;'
          '    property Visible;'
          '    property OnClick;'
          '    property Caption;'
          '    property Checked: Boolean read '
          'FChecked write SetChecked;'
          '    property Space: Integer read FSpace '
          'write SetSpace;'
          '  end;'
          ''
          '  TFsCheckBox = class'
          '(TFsCustomCheckBox)'
          '  private'
          '    FCheckedPicture: TFsPictureDrawable;'
          '    FUnCheckedPicture: TFsPictureDrawable;'
          '    procedure SetCheckedPicture(const '
          'Value: TPicture);'
          '    procedure SetUnCheckedPicture(const '
          'Value: TPicture);'
          '    function GetCheckedPicture: TPicture;'
          '    function GetUnCheckedPicture: TPicture;'
          '    procedure PictureChanged(Sender: '
          'TObject; ID: TNotifyID);'
          '  protected'
          '    procedure GetImageSize(out w, h: '
          'Integer); override;'
          '    procedure DrawMark(const Rect: TRect); '
          'override;'
          '  public'
          '    constructor Create(Owner: '
          'TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          '    property CheckedPicture: TPicture read '
          'GetCheckedPicture write SetCheckedPicture;'
          '    property UnCheckedPicture: TPicture '
          'read GetUnCheckedPicture write '
          'SetUnCheckedPicture;'
          '  end;'
          ''
          '  TFsSkinCheckBox = class'
          '(TFsCustomCheckBox)'
          '  '
          '  end;'
          ''
          'function GetDefaultScrollBar: '
          'TFsCustomScrollBar;'
          ''
          'implementation'
          ''
          'type'
          '  TControlHack = class(TControl)'
          '  '
          '  end;'
          ''
          'var'
          '  DefaultScrollBar: TFsCustomScrollBar;'
          ''
          'function GetDefaultScrollBar: '
          'TFsCustomScrollBar;'
          'begin'
          '  if not Assigned(DefaultScrollBar) then'
          '    DefaultScrollBar := '
          'TFsFlatScrollBar.Create(nil);'
          ''
          '  Result := DefaultScrollBar;'
          'end;'
          ''
          '{ TFsCustomButton }'
          ''
          'procedure '
          'TFsCustomButton.CMFontChanged(var '
          'msgr: TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.CMMouseEnter(var msg: '
          'TMessage);'
          'begin'
          '  inherited;'
          ''
          '  Include(FMouseFlag, mfMouseOver);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.CMMouseLeave(var msg: '
          'TMessage);'
          'begin'
          '  inherited;'
          ''
          '  Exclude(FMouseFlag, mfMouseOver);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.CMTextChange(var msg: '
          'TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'constructor TFsCustomButton.Create'
          '(Owner: TComponent);'
          'begin'
          '  inherited;'
          '  ControlStyle := ControlStyle + '
          '[csReplicatable, csPannable];'
          '  FSpace := 4;'
          '  FShowCaption := True;'
          '  FLayout := blTextInStretchPicture;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawButtonSurface;'
          'var'
          '  flags: DWORD;'
          'begin'
          '  flags := DFCS_BUTTONPUSH;'
          ''
          '  if mfLButtonDown in Self.FMouseFlag then'
          '    flags := flags or DFCS_PUSHED;'
          ''
          '  Windows.DrawFrameControl'
          '(Self.Canvas.Handle, Rect(0, 0, Self.Width, '
          'Self.Height),'
          '    DFC_BUTTON, flags);'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawImageAndText;'
          'var'
          '  w: Integer;'
          'begin'
          '  Self.GetPictureSize(w);'
          '  '
          '  if w = 0 then Self.DrawButtonSurface;'
          ''
          '  case Self.FLayout of'
          '    blPictureLeft: Self.DrawPictureLeft;'
          '    blPictureRight: Self.DrawPictureRight;'
          '    blPictureTop: Self.DrawPictureTop;'
          '    blPictureBottom: Self.DrawPictureBottom;'
          '    blTextInPicture: Self.DrawTextInPicture;'
          '    blTextInStretchPicture: '
          'Self.DrawTextInStretchPicture;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawPictureBottom;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  dim: TSize;'
          '  w, h: Integer;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    rpic.Bottom := Self.Height - (Self.Height - '
          'dim.cy) div 2;'
          '    rpic.Top := rpic.Bottom - h;'
          ''
          '    if PictureIsHorzStretchable and (dim.cx > '
          'w) then'
          '    begin'
          '      rpic.Left := (Self.Width - dim.cx) div 2;'
          '      rpic.Right := rpic.Left + dim.cx;'
          '    end'
          '    else begin'
          '      rpic.Left := (Self.Width - w) div 2;'
          '      rpic.Right := rpic.Left + w;'
          '    end;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Top := rpic.top - FSpace;'
          '  end'
          '  else rpic.Top := Self.Height - (Self.Height - '
          'dim.cy) div 2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') '
          'then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Top := (Self.Height - dim.cy) div 2;'
          '    rtext.Bottom := rpic.Top;'
          ''
          '    rtext.Left := (Self.Width - dim.cx) div 2;'
          '    rtext.Right := Self.Width - rtext.Left;'
          '    '
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          '    Canvas.TextRect(rtext, Text, [tfCenter, '
          'tfVerticalCenter, tfSingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawPictureLeft;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  dim: TSize;'
          '  w, h: Integer;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    rpic.Left := (Self.Width - dim.cx) div 2;'
          ''
          '    if PictureIsVertStretchable and (dim.cy > '
          'h) then'
          '    begin'
          '      rpic.Top := (Self.Height - dim.cy) div 2;'
          '      rpic.Bottom := rpic.Top + dim.cy;'
          '    end'
          '    else begin'
          '      rpic.Top := (Self.Height - h) div 2;'
          '      rpic.Bottom := rpic.Top + h;'
          '    end;'
          ''
          '    rpic.Right := rpic.Left + w;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Right := rpic.Right + FSpace;'
          '  end'
          '  else rpic.Right := (Self.Width - dim.cx) div '
          '2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') '
          'then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Left := rpic.Right;'
          '    rtext.Right := Self.Width - (Self.Width - '
          'dim.cx) div 2;'
          '    rtext.Top := (Self.Height - dim.cy) div 2;'
          '    rtext.Bottom := Self.Height - (Self.Height - '
          'dim.cy) div 2;'
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          '    Canvas.TextRect(rtext, Text, [tfCenter, '
          'tfVerticalCenter, tfSingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawPictureRight;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  w, h: Integer;'
          '  dim: TSize;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          '  '
          '  if w > 0 then'
          '  begin'
          '    rpic.Right := Self.Width - (Self.Width - '
          'dim.cx) div 2;'
          ''
          '    if PictureIsVertStretchable and (dim.cy > '
          'h) then'
          '    begin'
          '      rpic.Top := (Self.Height - dim.cy) div 2;'
          '      rpic.Bottom := rpic.Top + dim.cy;'
          '    end'
          '    else begin'
          '      rpic.Top := (Self.Height - h) div 2;'
          '      rpic.Bottom := rpic.Top + h;'
          '    end;'
          ''
          '    rpic.Left := rpic.Right - w;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Left := rpic.Left - FSpace;'
          '  end'
          '  else rpic.Left := Self.Width - (Self.Width - '
          'dim.cx) div 2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') '
          'then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Left := (Self.Width - dim.cx) div 2;'
          '    rtext.Right := rpic.Left;'
          '    rtext.Top := (Self.Height - dim.cy) div 2;'
          '    rtext.Bottom := Self.Height - (Self.Height - '
          'dim.cy) div 2;'
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          '    Canvas.TextRect(rtext, Text, [tfCenter, '
          'tfVerticalCenter, tfSingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawPictureTop;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  w, h: Integer;'
          '  dim: TSize;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    rpic.Top := (Self.Height - dim.cy) div 2;'
          '    rpic.Bottom := rpic.Top + h;'
          ''
          '    if PictureIsHorzStretchable and (dim.cx > '
          'w) then'
          '    begin'
          '      rpic.Left := (Self.Width - dim.cx) div 2;'
          '      rpic.Right := rpic.Left + dim.cx;'
          '    end'
          '    else begin'
          '      rpic.Left := (Self.Width - w) div 2;'
          '      rpic.Right := rpic.Left + w;'
          '    end;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Bottom := rpic.Bottom + FSpace;'
          '  end'
          '  else rpic.Bottom := (Self.Height - dim.cy) '
          'div 2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') '
          'then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Top := rpic.Bottom;'
          '    rtext.Bottom := Self.Height - (Self.Height - '
          'dim.cy) div 2;'
          ''
          '    rtext.Left := (Self.Width - dim.cx) div 2;'
          '    rtext.Right := Self.Width - rtext.Left;'
          '    '
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          '    Canvas.TextRect(rtext, Text, [tfCenter, '
          'tfVerticalCenter, tfSingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawTextInPicture;'
          'var'
          '  Text: string;'
          '  r: TRect;'
          '  w, h: Integer;'
          '  dim: TSize;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    if PictureIsHorzStretchable and (w < '
          'dim.cx) then'
          '      r.Left := (Self.Width - dim.cx) div 2'
          '    else'
          '      r.Left := (Self.Width - w) div 2;'
          ''
          '    r.Right := Self.Width - r.Left;'
          ''
          '    if PictureIsVertStretchable and (h < '
          'dim.cy) then'
          '      r.Top := (Self.Height - dim.cy) div 2'
          '    else'
          '      r.Top := (Self.Height - h) div 2;'
          ''
          '    r.Bottom := Self.Height - r.Top;'
          '    '
          '    Self.DrawPicture(r);'
          '  end;'
          ''
          '  if FShowCaption then'
          '  begin'
          '    Text := Caption;'
          ''
          '    if Text <> '#39#39' then'
          '    begin'
          '      r.Left := 0;'
          '      r.Right := Self.Width;'
          '      r.Top := 0;'
          '      r.Bottom := Self.Height;'
          ''
          '      Canvas.Font := Self.Font;'
          '      Canvas.Brush.Style := bsClear;'
          '      Canvas.TextRect(r, Text, [tfCenter, '
          'tfVerticalCenter, tfSingleLine]);'
          '    end;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.DrawTextInStretchPicture'
          ';'
          'var'
          '  Text: string;'
          '  r: TRect;'
          '  w: Integer;'
          'begin'
          '  r.Left := 0;'
          '  r.Right := Self.Width;'
          '  r.Top := 0;'
          '  r.Bottom := Self.Height;'
          ''
          '  Self.GetPictureSize(w);'
          ''
          '  if w > 0 then Self.DrawPicture(r);'
          ''
          '  if FShowCaption then'
          '  begin'
          '    Text := Caption;'
          ''
          '    if Text <> '#39#39' then'
          '    begin'
          '      Canvas.Font := Self.Font;'
          '      Canvas.Brush.Style := bsClear;'
          '      Canvas.TextRect(r, Text, [tfCenter, '
          'tfVerticalCenter, tfSingleLine]);'
          '    end;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.GetContentDimension'
          '(out dim: TSize);'
          'var'
          '  TextSize: TSize;'
          '  ImageSize: TSize;'
          'begin'
          '  dim.cx := 0;'
          '  dim.cy := 0;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') '
          'then'
          '  begin'
          '    Self.Canvas.Font := Self.Font;'
          '    TextSize := Self.Canvas.TextExtent'
          '(Self.Caption);'
          '  end'
          '  else begin'
          '    TextSize.cx := 0;'
          '    TextSize.cy := 0;'
          '  end;'
          ''
          '  ImageSize.cy := Self.GetPictureSize'
          '(ImageSize.cx);'
          ''
          '  if (ImageSize.cx = 0) and (TextSize.cx = 0) '
          'then Exit;'
          ''
          '  case FLayout of'
          '    blPictureLeft, blPictureRight:'
          '      begin'
          '        if ImageSize.cx > 0 then'
          '        begin'
          '          if TextSize.cx > 0 then dim.cx := '
          'ImageSize.cx + FSpace + TextSize.cx'
          '          else dim.cx := ImageSize.cx;'
          '        end'
          '        else begin'
          '          if TextSize.cx > 0 then dim.cx := '
          'TextSize.cx'
          '          else dim.cx := 0;'
          '        end;'
          ''
          '        if TextSize.cy > ImageSize.cy then '
          'dim.cy := TextSize.cy'
          '        else dim.cy := ImageSize.cy;'
          '      end;'
          ''
          '    blPictureTop, blPictureBottom:'
          '      begin'
          '        if ImageSize.cy > 0 then'
          '        begin'
          '          if TextSize.cy > 0 then dim.cy := '
          'ImageSize.cy + FSpace + TextSize.cy'
          '          else dim.cy := ImageSize.cy;'
          '        end'
          '        else begin'
          '          if TextSize.cy > 0 then dim.cy := '
          'TextSize.cy'
          '          else dim.cy := 0;'
          '        end;'
          ''
          '        if TextSize.cx > ImageSize.cx then '
          'dim.cx := TextSize.cx'
          '        else dim.cx := ImageSize.cx;'
          '      end;'
          ''
          '    blTextInPicture, blTextInStretchPicture:'
          '      begin'
          '        if TextSize.cx > ImageSize.cx then '
          'dim.cx := TextSize.cx'
          '        else dim.cx := ImageSize.cx;'
          ''
          '        if TextSize.cy > ImageSize.cy then '
          'dim.cy := TextSize.cy'
          '        else dim.cy := ImageSize.cy;'
          '      end;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.MouseDown'
          '(Button: TMouseButton; Shift: TShiftState; '
          'X, Y: Integer);'
          'begin'
          '  inherited;'
          ''
          '  if Button = mbLeft then Include'
          '(FMouseFlag, mfLButtonDown);'
          ''
          '  if Button = mbRight then Include'
          '(FMouseFlag, mfRButtonDown);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCustomButton.MouseUp'
          '(Button: TMouseButton; Shift: TShiftState; '
          'X, Y: Integer);'
          'begin'
          '  inherited;'
          '  '
          '  if Button = mbLeft then Exclude'
          '(FMouseFlag, mfLButtonDown);'
          ''
          '  if Button = mbRight then Exclude'
          '(FMouseFlag, mfRButtonDown);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCustomButton.Paint;'
          'begin'
          '  Self.DrawImageAndText;'
          'end;'
          ''
          'function '
          'TFsCustomButton.PictureIsHorzStretchable: '
          'Boolean;'
          'begin'
          '  Result := False;'
          'end;'
          ''
          'function '
          'TFsCustomButton.PictureIsVertStretchable: '
          'Boolean;'
          'begin'
          '  Result := False;'
          'end;'
          ''
          'procedure TFsCustomButton.SetLayout'
          '(const Value: TFsImageButtonLayout);'
          'begin'
          '  if FLayout <> Value then'
          '  begin'
          '    FLayout := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.SetSpace'
          '(const Value: Integer);'
          'begin'
          '  if (FSpace <> Value) and (Value >= 0) then'
          '  begin'
          '    FSpace := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomButton.SetShowCaption(const '
          'Value: Boolean);'
          'begin'
          '  if FShowCaption <> Value then'
          '  begin'
          '    FShowCaption := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          '{ TFsImageButton }'
          ''
          'constructor TFsImageButton.Create(Owner: '
          'TComponent);'
          'begin'
          '  inherited;'
          '  FPicture := TFsPictureDrawable.Create'
          '(Self);'
          '  FDisablePicture := '
          'TFsPictureDrawable.Create(Self);'
          '  FMouseOverPicture := '
          'TFsPictureDrawable.Create(Self);'
          '  FMouseDownPicture := '
          'TFsPictureDrawable.Create(Self);'
          '  FPicture.AddOnChangeListener'
          '(Self.PictureChanged);'
          '  FDisablePicture.AddOnChangeListener'
          '(Self.PictureChanged);'
          '  FMouseOverPicture.AddOnChangeListener'
          '(Self.PictureChanged);'
          '  '
          'FMouseDownPicture.AddOnChangeListener'
          '(Self.PictureChanged);'
          'end;'
          ''
          'destructor TFsImageButton.Destroy;'
          'begin'
          '  FPicture.RemoveOnChangeListener'
          '(Self.PictureChanged);'
          '  FDisablePicture.RemoveOnChangeListener'
          '(Self.PictureChanged);'
          '  '
          'FMouseOverPicture.RemoveOnChangeListen'
          'er(Self.PictureChanged);'
          '  '
          'FMouseDownPicture.RemoveOnChangeListe'
          'ner(Self.PictureChanged);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsImageButton.DrawPicture'
          '(const Rect: TRect);'
          'var'
          '  drawable: TFsSingleDrawable;'
          'begin'
          '  drawable := Self.GetDrawable;'
          ''
          '  if Assigned(drawable) then'
          '    drawable.Draw(Self.Canvas, Rect);'
          'end;'
          ''
          'function TFsImageButton.GetDisablePicture: '
          'TPicture;'
          'begin'
          '  Result := FDisablePicture.Picture;'
          'end;'
          ''
          'function TFsImageButton.GetDrawable: '
          'TFsSingleDrawable;'
          'begin'
          '  if not Self.Enabled then Result := '
          'FDisablePicture'
          '  else if mfLButtonDown in FMouseFlag then '
          'Result := FMouseDownPicture'
          '  else if mfMouseOver in FMouseFlag then '
          'Result := FMouseOverPicture'
          '  else Result := FPicture;'
          ''
          '  if Result.Empty then'
          '    Result := FPicture;'
          'end;'
          ''
          'function '
          'TFsImageButton.GetMouseDownPicture: '
          'TPicture;'
          'begin'
          '  Result := FMouseDownPicture.Picture;'
          'end;'
          ''
          'function '
          'TFsImageButton.GetMouseOverPicture: '
          'TPicture;'
          'begin'
          '  Result := FMouseOverPicture.Picture;'
          'end;'
          ''
          'function TFsImageButton.GetPicture: '
          'TPicture;'
          'begin'
          '  Result := FPicture.Picture;'
          'end;'
          ''
          'function TFsImageButton.GetPictureSize(out '
          'width: Integer): Integer;'
          'var'
          '  drawable: TFsDrawable;'
          'begin'
          '  drawable := Self.GetDrawable;'
          ''
          '  if Assigned(drawable) and not '
          'drawable.Empty then'
          '  begin'
          '    width := drawable.Width;'
          '    Result := drawable.Height;'
          '  end'
          '  else begin'
          '    width := 0;'
          '    Result := 0;'
          '  end;'
          'end;'
          ''
          'procedure TFsImageButton.PictureChanged'
          '(Sender: TObject; ID: TNotifyID);'
          'begin'
          '  if (ID = niChange) and (Sender = '
          'GetDrawable) then '
          'Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsImageButton.SetPicture(const '
          'Value: TPicture);'
          'begin'
          '  FPicture.Picture.Assign(Value);'
          'end;'
          ''
          'procedure '
          'TFsImageButton.SetDisablePicture(const '
          'Value: TPicture);'
          'begin'
          '  FDisablePicture.Picture.Assign(Value);'
          'end;'
          ''
          'procedure '
          'TFsImageButton.SetMouseDownPicture'
          '(const Value: TPicture);'
          'begin'
          '  FMouseDownPicture.Picture.Assign(Value);'
          'end;'
          ''
          'procedure '
          'TFsImageButton.SetMouseOverPicture'
          '(const Value: TPicture);'
          'begin'
          '  FMouseOverPicture.Picture.Assign(Value);'
          'end;'
          ''
          '{ TFsImage }'
          ''
          'constructor TFsImage.Create(Owner: '
          'TComponent);'
          'begin'
          '  inherited;'
          '  ControlStyle := ControlStyle + '
          '[csReplicatable, csPannable];'
          '  Self.SetBounds(Left, Top, 100, 100);'
          'end;'
          ''
          'destructor TFsImage.Destroy;'
          'begin'
          '  SetPicture(nil);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsImage.GetContentDimension'
          '(out dim: TSize);'
          'begin'
          '  if Assigned(FPicture) then'
          '  begin'
          '    dim.cx := FPicture.Width;'
          '    dim.cy := FPicture.Height;'
          '  end'
          '  else begin'
          '    dim.cx := 0;'
          '    dim.cy := 0;'
          '  end;'
          'end;'
          ''
          'procedure TFsImage.Paint;'
          'var'
          '  r: TRect;'
          'begin'
          '  inherited;'
          ''
          '  r := Rect(0, 0, Width, Height);'
          ''
          '  if Assigned(FPicture) and not '
          'FPicture.Empty then'
          '  begin'
          '    if FPicture is TFsSingleDrawable then'
          '      TFsSingleDrawable(FPicture).Draw'
          '(Canvas, r)'
          '    else'
          '      TFsMultiFrameDrawable'
          '(FPicture).DrawFrame(Canvas.Handle, r, 0);'
          '  end;'
          ''
          '  if csDesigning in ComponentState then'
          '  begin'
          '    Canvas.Pen.Style := psDash;'
          '    Canvas.Brush.Style := bsClear;'
          '    Canvas.Rectangle(0, 0, Width, Height);'
          '  end;'
          'end;'
          ''
          'procedure TFsImage.PictureChanged'
          '(Sender: TObject; ID: TNotifyID);'
          'begin'
          '  if ID = niDestroy then'
          '  begin'
          '    if Sender = FPicture then'
          '    begin'
          '      FPicture := nil;'
          '      Self.AutoSizeAndInvalidate;'
          '    end;'
          '  end'
          '  else if ID = niChange then'
          '  begin'
          '    if Sender = FPicture then '
          'Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsImage.SetPicture(const Value: '
          'TFsDrawable);'
          'begin'
          '  if FPicture <> Value then'
          '  begin'
          '    if Assigned(FPicture) then'
          '      FPicture.RemoveOnChangeListener'
          '(Self.PictureChanged);'
          ''
          '    FPicture := Value;'
          ''
          '    if Assigned(FPicture) then'
          '      FPicture.AddOnChangeListener'
          '(Self.PictureChanged);'
          '      '
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          '{ TFsEdit }'
          ''
          'procedure TFsEdit.CMMouseEnter(var msg: '
          'TMessage);'
          'begin'
          '  FMouseIn := True;'
          '  inherited;'
          'end;'
          ''
          'procedure TFsEdit.CMMouseLeave(var msg: '
          'TMessage);'
          'begin'
          '  FMouseIn := False;'
          '  inherited;'
          'end;'
          ''
          'constructor TFsEdit.Create(AOwner: '
          'TComponent);'
          'begin'
          '  inherited;'
          '  FMouseInBorderColor := RGB(123, 228, '
          '255);'
          'end;'
          ''
          'procedure TFsEdit.SetMouseInBorderColor'
          '(const Value: TColor);'
          'begin'
          '  if FMouseInBorderColor <> Value then'
          '  begin'
          '    FMouseInBorderColor := Value;'
          '    Self.Invalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsEdit.WMNCPAINT(var msg: '
          'TWMNCPaint);'
          'var'
          '  dc: HDC;'
          '  hb: HBRUSH;'
          '  bgc: DWORD;'
          'begin'
          '  dc := GetWindowDC(Handle);'
          ''
          '  try    '
          '    if FMouseIn then bgc := ColorToRGB'
          '(FMouseInBorderColor)'
          '    else bgc := RGB(78, 160, 209);'
          '    '
          '    hb := CreateSolidBrush(bgc);'
          '    Windows.FrameRect(dc, Rect(0, 0, '
          'Self.Width, Self.Height), hb);'
          '    DeleteObject(hb);'
          ''
          '    if FMouseIn then bgc := RGB(78, 160, '
          '209)'
          '    else bgc := ColorToRGB(Self.Color);'
          '    '
          '    hb := CreateSolidBrush(bgc);'
          '    Windows.FrameRect(dc, Rect(1, 1, '
          'Self.Width - 1, Self.Height - 1), hb);'
          '    DeleteObject(hb);'
          '  finally'
          '    ReleaseDC(Handle, dc);'
          '  end;'
          'end;'
          ''
          '{ TFsCustomCheckBox }'
          ''
          'procedure '
          'TFsCustomCheckBox.CMFontChange(var '
          'msg: TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure '
          'TFsCustomCheckBox.CMTextChange(var '
          'msg: TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'constructor TFsCustomCheckBox.Create'
          '(Owner: TComponent);'
          'begin'
          '  inherited;'
          '  FSpace := 4;'
          '  ControlStyle := ControlStyle + '
          '[csReplicatable, csPannable];'
          'end;'
          ''
          'procedure '
          'TFsCustomCheckBox.GetContentDimension'
          '(out dim: TSize);'
          'var'
          '  TextSize: TSize;'
          '  ImageWidth, ImageHeight: Integer;'
          'begin'
          '  Canvas.Font := Self.Font;'
          ''
          '  TextSize := Canvas.TextExtent'
          '(Self.Caption);'
          ''
          '  Self.GetImageSize(ImageWidth, '
          'ImageHeight);'
          ''
          '  if ImageWidth = 0 then'
          '  begin'
          '    ImageWidth := '
          'Windows.GetSystemMetrics'
          '(SM_CXMENUCHECK);'
          '    ImageHeight := '
          'Windows.GetSystemMetrics'
          '(SM_CYMENUCHECK);'
          '  end;'
          ''
          '  dim.cx := ImageWidth + FSpace + '
          'TextSize.cx;'
          ''
          '  if TextSize.cy > ImageHeight then dim.cy '
          ':= TextSize.cy'
          '  else dim.cy := ImageHeight;'
          'end;'
          ''
          'procedure '
          'TFsCustomCheckBox.GetImageSize(out w, '
          'h: Integer);'
          'begin'
          '  w := 0;'
          '  h := 0;'
          'end;'
          ''
          'procedure '
          'TFsCustomCheckBox.MouseDown(Button: '
          'TMouseButton; Shift: TShiftState; X, Y: '
          'Integer);'
          'begin'
          '  if Button = mbLeft then'
          '  begin'
          '    FChecked := not FChecked;'
          '    Invalidate;'
          '  end;'
          ''
          '  inherited;'
          'end;'
          ''
          'procedure TFsCustomCheckBox.Paint;'
          'var'
          '  w, h: Integer;'
          '  r: TRect;'
          '  _caption: string;'
          'begin'
          '  inherited;'
          ''
          '  Self.GetImageSize(w, h);'
          ''
          '  if w = 0 then'
          '  begin'
          '    w := Windows.GetSystemMetrics'
          '(SM_CXMENUCHECK);'
          '    h := Windows.GetSystemMetrics'
          '(SM_CYMENUCHECK);'
          ''
          '    r.Left := 0;'
          '    r.Top := (Self.Height - h) div 2;'
          '    r.Right := w;'
          '    r.Bottom := r.Top + h;'
          ''
          '    if Self.FChecked then'
          '      Windows.DrawFrameControl'
          '(Canvas.Handle, r, DFC_BUTTON, '
          'DFCS_BUTTONCHECK or DFCS_CHECKED)'
          '    else'
          '      Windows.DrawFrameControl'
          '(Canvas.Handle, r, DFC_BUTTON, '
          'DFCS_BUTTONCHECK);'
          '  end'
          '  else begin'
          '    r.Left := 0;'
          '    r.Top := (Self.Height - h) div 2;'
          '    r.Right := w;'
          '    r.Bottom := r.Top + h;'
          '    Self.DrawMark(r);'
          '  end;'
          ''
          '  r := Rect(r.Right + FSpace, 0, Self.Width, '
          'Self.Height);'
          ''
          '  Self.Canvas.Font := Self.Font;'
          '  Self.Canvas.Brush.Style := bsClear;'
          '  SetBkMode(Self.Canvas.Handle, '
          'Windows.TRANSPARENT);'
          '  _caption := Self.Caption;'
          '  '
          '  Self.Canvas.TextRect(r, _caption, '
          '[tfVerticalCenter, tfSingleLine]);'
          'end;'
          ''
          'procedure '
          'TFsCustomCheckBox.SetChecked(const '
          'Value: Boolean);'
          'begin'
          '  if FChecked <> Value then'
          '  begin'
          '    FChecked := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomCheckBox.SetSpace'
          '(const Value: Integer);'
          'begin'
          '  if (FSpace <> Value) and (Value >= 0) then'
          '  begin'
          '    FSpace := Value;'
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          '{ TFsCheckBox }'
          ''
          'constructor TFsCheckBox.Create(Owner: '
          'TComponent);'
          'begin'
          '  inherited;'
          '  FCheckedPicture := '
          'TFsPictureDrawable.Create(Self);'
          '  FUnCheckedPicture := '
          'TFsPictureDrawable.Create(Self);'
          '  FCheckedPicture.AddOnChangeListener'
          '(Self.PictureChanged);'
          '  FUnCheckedPicture.AddOnChangeListener'
          '(Self.PictureChanged);'
          'end;'
          ''
          'destructor TFsCheckBox.Destroy;'
          'begin'
          '  '
          'FCheckedPicture.RemoveOnChangeListener'
          '(Self.PictureChanged);'
          '  '
          'FUnCheckedPicture.RemoveOnChangeListen'
          'er(Self.PictureChanged);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsCheckBox.DrawMark(const '
          'Rect: TRect);'
          'begin'
          '  if Self.Checked then'
          '    FCheckedPicture.Draw(Self.Canvas, Rect)'
          '  else'
          '    FUnCheckedPicture.Draw(Self.Canvas, '
          'Rect)'
          'end;'
          ''
          'function TFsCheckBox.GetCheckedPicture: '
          'TPicture;'
          'begin'
          '  Result := FCheckedPicture.Picture;'
          'end;'
          ''
          'procedure TFsCheckBox.GetImageSize(out '
          'w, h: Integer);'
          'begin'
          '  if Self.Checked then'
          '  begin'
          '    w := FCheckedPicture.Width;'
          '    h := FCheckedPicture.Height;'
          '  end'
          '  else begin'
          '    w := FUnCheckedPicture.Width;'
          '    h := FUnCheckedPicture.Height;'
          '  end;'
          'end;'
          ''
          'function '
          'TFsCheckBox.GetUnCheckedPicture: '
          'TPicture;'
          'begin'
          '  Result := FUnCheckedPicture.Picture;'
          'end;'
          ''
          'procedure TFsCheckBox.PictureChanged'
          '(Sender: TObject; ID: TNotifyID);'
          'begin'
          '  if ID = niChange then'
          '  begin'
          '    if ((Sender = FCheckedPicture) and '
          'FChecked) or ((Sender = '
          'FUnCheckedPicture) and not FChecked) '
          'then'
          '      Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCheckBox.SetCheckedPicture'
          '(const Value: TPicture);'
          'begin'
          '  FCheckedPicture.Picture.Assign(Value);'
          'end;'
          ''
          'procedure '
          'TFsCheckBox.SetUnCheckedPicture(const '
          'Value: TPicture);'
          'begin'
          '  FUnCheckedPicture.Picture.Assign(Value);'
          'end;'
          ''
          '{ TFsButtonEdit }'
          ''
          'constructor TFsButtonEdit.Create(AOwner: '
          'TComponent);'
          'begin'
          '  inherited;'
          '  FButtonPicture := TPicture.Create;'
          '  FNCCanvas := TCanvas.Create;'
          'end;'
          ''
          'destructor TFsButtonEdit.Destroy;'
          'begin'
          '  FButtonPicture.Free;'
          '  FNCCanvas.Free;'
          '  inherited;'
          'end;'
          ''
          'procedure TFsButtonEdit.WMNCCalcSize(var '
          'msgr: TWMNCCalcSize);'
          'begin'
          '  with msgr.CalcSize_Params^ do'
          '  begin'
          '    rgrc[0].Left := rgrc[0].Left + 2;'
          '    rgrc[0].Top := rgrc[0].Top + 2;'
          '    rgrc[0].Right := rgrc[0].Right - 2 - '
          'FButtonPicture.Width;'
          '    rgrc[0].Bottom := rgrc[0].Bottom - 2;'
          '  end;'
          ''
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsButtonEdit.WMNCHitTest(var '
          'msgr: TWMNCHitTest);'
          'var'
          '  pt: TPoint;'
          'begin'
          '  pt := Self.ScreenToClient(Point(msgr.XPos, '
          'msgr.YPos));'
          ''
          '  if PtInRect(Rect(Self.Width - 18, 2, '
          'Self.Width - 2, Self.Height - 2), pt) then '
          'msgr.Result := HTBORDER'
          '  else inherited;'
          'end;'
          ''
          'procedure '
          'TFsButtonEdit.WMNCLButtonDown(var '
          'msgr: TWMNCLButtonDown);'
          'var'
          '  pt: TPoint;'
          'begin'
          '  pt := Self.ScreenToClient(Point'
          '(msgr.XCursor, msgr.YCursor));'
          ''
          '  if PtInRect(Rect(Self.Width - 18, 2, '
          'Self.Width - 2, Self.Height - 2), pt) then'
          '  begin'
          '    if Assigned(FOnClickButton) then'
          '      FOnClickButton(Self);'
          ''
          '    msgr.Result := 0;'
          '  end'
          '  else inherited;'
          'end;'
          ''
          'procedure TFsButtonEdit.WMNCPAINT(var '
          'msgr: TWMNCPaint);'
          'var'
          '  dc: HDC;'
          '  r: TRect;'
          'begin'
          '  inherited;'
          ''
          '  r.Left := Self.Width - 2 - '
          'FButtonPicture.Width;'
          '  r.Right := Self.Width - 2;'
          ''
          '  r.Top := (Self.Height - '
          'FButtonPicture.Height) div 2;'
          '  r.Bottom := Self.Height - (Self.Height - '
          'FButtonPicture.Height) div 2;'
          ''
          '  dc := GetWindowDC(Self.Handle);'
          '  '
          '  FNCCanvas.Handle := dc;'
          ''
          '  try'
          '    FNCCanvas.Brush.Color := Self.Color;'
          '    FNCCanvas.FillRect(Rect(r.Left, 2, r.Right, '
          'Self.Height - 2));'
          '    '
          '    if Assigned(FButtonPicture.Graphic) then'
          '      FNCCanvas.StretchDraw(r, '
          'FButtonPicture.Graphic);'
          '  finally'
          '    FNCCanvas.Handle := 0;'
          '    ReleaseDC(Self.Handle, dc);'
          '  end;  '
          'end;'
          ''
          'procedure '
          'TFsButtonEdit.WriteButtonPicture(const '
          'Value: TPicture);'
          'begin'
          '  FButtonPicture.Assign(Value);'
          'end;'
          ''
          '{ TFsNCScrollMemo }'
          ''
          'procedure '
          'TFsNCScrollMemo.CMMouseEnter(var msg: '
          'TMessage);'
          'begin'
          '  inherited;'
          '  FMouseIn := True;'
          'end;'
          ''
          'procedure '
          'TFsNCScrollMemo.CMMouseLeave(var msg: '
          'TMessage);'
          'begin'
          '  inherited;'
          '  FMouseIn := False;'
          'end;'
          ''
          'constructor TFsNCScrollMemo.Create'
          '(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FMouseInBorderColor := RGB(123, 228, '
          '255);'
          'end;'
          ''
          'function TFsNCScrollMemo.GetHScrollRect'
          '(var rc: TRect): Boolean;'
          'var'
          '  style: Integer;'
          'begin'
          '  style := GetWindowLong(Handle, '
          'GWL_STYLE);'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    rc.Bottom := Self.Height - 2;'
          '    rc.Top := rc.Bottom - '
          'GetRealScrollBar.HScrollHeight;'
          '    rc.Left := 2;'
          ''
          '    if style and WS_VSCROLL = 0 then '
          'rc.Right := Self.Width - 2'
          '    else rc.Right := Self.Width - 2 - '
          'GetRealScrollBar.VScrollWidth;'
          ''
          '    Result := True;'
          '  end'
          '  else Result := False;'
          'end;'
          ''
          'function TFsNCScrollMemo.GetRealScrollBar: '
          'TFsCustomScrollBar;'
          'begin'
          '  if Assigned(FScrollBar) then Result := '
          'FScrollBar'
          '  else Result := GetDefaultScrollBar;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.GetScrollRect'
          '(var rcVScroll, rcHScroll, rcIntersect: TRect);'
          'var'
          '  style: Integer;'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  sb := GetRealScrollBar;'
          '  style := GetWindowLong(Handle, '
          'GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    rcVScroll.Right := Self.Width - 2;'
          '    rcVScroll.Left := rcVScroll.Right - '
          'sb.VScrollWidth;'
          '    rcVScroll.Top := 2;'
          ''
          '    if style and WS_HSCROLL = 0 then '
          'rcVScroll.Bottom := Self.Height - 2'
          '    else rcVScroll.Bottom := Self.Height - 2 - '
          'sb.HScrollHeight;'
          '  end'
          '  else begin'
          '    rcVScroll.Left := 0;'
          '    rcVScroll.Right := -1;'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    rcHScroll.Bottom := Self.Height - 2;'
          '    rcHScroll.Top := rcHScroll.Bottom - '
          'sb.HScrollHeight;'
          '    rcHScroll.Left := 2;'
          ''
          '    if style and WS_VSCROLL = 0 then '
          'rcHScroll.Right := Self.Width - 2'
          '    else rcHScroll.Right := Self.Width - 2 - '
          'sb.VScrollWidth;'
          '  end'
          '  else begin'
          '    rcHScroll.Left := 0;'
          '    rcHScroll.Right := -1;'
          '  end;'
          ''
          '  if (style and WS_HSCROLL <> 0) and '
          '(style and WS_VSCROLL <> 0) then'
          '  begin'
          '    rcIntersect.Left := Self.Width - 2 - '
          'sb.VScrollWidth;'
          '    rcIntersect.Right := Self.Width - 2;'
          '    rcIntersect.Top := Self.Height - 2 - '
          'sb.HScrollHeight;'
          '    rcIntersect.Bottom := Self.Height - 2;'
          '  end'
          '  else begin'
          '    rcIntersect.Left := 0;'
          '    rcIntersect.Right := -1;'
          '  end;'
          '  '
          'end;'
          ''
          'function TFsNCScrollMemo.GetVScrollRect'
          '(var rc: TRect): Boolean;'
          'var'
          '  style: Integer;'
          'begin'
          '  style := GetWindowLong(Handle, '
          'GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    rc.Right := Self.Width - 2;'
          '    rc.Left := rc.Right - '
          'GetRealScrollBar.VScrollWidth;'
          '    rc.Top := 2;'
          ''
          '    if style and WS_HSCROLL = 0 then '
          'rc.Bottom := Self.Height - 2'
          '    else rc.Bottom := Self.Height - 2 - '
          'GetRealScrollBar.HScrollHeight;'
          ''
          '    Result := True;'
          '  end'
          '  else Result := False;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.Notification'
          '(AComponent: TComponent; Operation: '
          'TOperation);'
          'begin'
          '  inherited;'
          ''
          '  if (Operation = opRemove) and '
          '(AComponent = FScrollBar) then'
          '    SetScrollBar(nil);'
          'end;'
          ''
          'procedure TFsNCScrollMemo.PaintNC;'
          'var'
          '  dc: HDC;'
          '  hb: HBRUSH;'
          '  r, rcIntersect, rcVScroll, rcHScroll: TRect;'
          '  bgc: DWORD;'
          '  si: TScrollInfo;'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  dc := GetWindowDC(Handle);'
          ''
          '  try'
          '    if FMouseIn then bgc := ColorToRGB'
          '(FMouseInBorderColor)'
          '    else bgc := RGB(78, 160, 209);'
          ''
          '    r.Left := 0;'
          '    r.Top := 0;'
          '    r.Right := Self.Width;'
          '    r.Bottom := Self.Height;'
          ''
          '    hb := CreateSolidBrush(bgc);'
          '    Windows.FrameRect(dc, r, hb);'
          '    DeleteObject(hb);'
          ''
          '    if FMouseIn then bgc := RGB(78, 160, '
          '209)  '
          '    else bgc := ColorToRGB(Self.Color);'
          ''
          '    r.Left := 1;'
          '    r.Top := 1;'
          '    r.Right := Self.Width - 1;'
          '    r.Bottom := Self.Height - 1;'
          ''
          '    hb := CreateSolidBrush(bgc);'
          '    FrameRect(dc, r, hb);'
          '    DeleteObject(hb);'
          ''
          '    sb := GetRealScrollBar;'
          '    '
          '    Self.GetScrollRect(rcVScroll, rcHScroll, '
          'rcIntersect);'
          ''
          '    if rcVScroll.Left < rcVScroll.Right then'
          '    begin'
          '      si.cbSize := SizeOf(si);'
          '      si.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '      Windows.GetScrollInfo(Handle, '
          'SB_VERT, si);'
          '      //sb.DrawVScroll(dc, si, rcVScroll);'
          '    end;'
          ''
          '    if rcHScroll.Left < rcHScroll.Right then'
          '    begin'
          '      si.cbSize := SizeOf(si);'
          '      si.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '      Windows.GetScrollInfo(Handle, '
          'SB_HORZ, si);'
          '      //sb.DrawHScroll(dc, si, rcHScroll);'
          '    end;'
          ''
          '    if rcIntersect.Left < rcIntersect.Right then'
          '      sb.DrawIntersect(dc, rcIntersect);'
          ''
          '  finally'
          '    ReleaseDC(Handle, dc);'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsNCScrollMemo.SetMouseInBorderColor'
          '(const Value: TColor);'
          'begin'
          '  if FMouseInBorderColor <> Value then'
          '  begin'
          '    FMouseInBorderColor := Value;'
          '    Self.Invalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.SetScrollBar'
          '(const Value: TFsCustomScrollBar);'
          'begin'
          '  if Value <> FScrollBar then'
          '  begin'
          '    if Assigned(FScrollBar) then'
          '      FScrollBar.RemoveFreeNotification(Self);'
          ''
          '    FScrollBar := Value;'
          ''
          '    if Assigned(FScrollBar) then'
          '      FScrollBar.FreeNotification(Self);'
          ''
          '    Self.RecreateWnd;'
          '  end;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WMNCCalcSize'
          '(var msgr: TWMNCCalcSize);'
          'var'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  with msgr.CalcSize_Params^ do'
          '  begin'
          '    rgrc[0].Left := rgrc[0].Left + 2;'
          '    rgrc[0].Top := rgrc[0].Top + 2;'
          '    rgrc[0].Right := rgrc[0].Right - 2;'
          '    rgrc[0].Bottom := rgrc[0].Bottom - 2;'
          ''
          '    sb := Self.GetRealScrollBar;'
          '    '
          '    if (GetWindowLong(Self.Handle, '
          'GWL_STYLE) and WS_VSCROLL) <> 0 then'
          '      Dec(rgrc[0].Right, sb.VScrollWidth);'
          ''
          '    if (GetWindowLong(Self.Handle, '
          'GWL_STYLE) and WS_HSCROLL) <> 0 '
          'then'
          '      Dec(rgrc[0].Bottom, sb.HScrollHeight);'
          '  end;'
          ''
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WMNCHitTest'
          '(var msgr: TWMNCHitTest);'
          'var'
          '  pt: TPoint;'
          '  rcClient: TRect;'
          'begin'
          '  inherited;'
          ''
          '  pt.X := msgr.XPos;'
          '  pt.Y := msgr.YPos;'
          '  Windows.ScreenToClient(Self.Handle, pt);'
          ''
          '  Windows.GetClientRect(Self.Handle, '
          'rcClient);'
          ''
          '  if PtInRect(rcClient, pt) then msgr.Result := '
          'HTCLIENT'
          '  else begin'
          '    //PtInRect(Rect(rcClient.Right, 0, '
          'rcClient.Right + GetRealScrollBar.))'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsNCScrollMemo.WMNCLButtonDown(var '
          'msgr: TWMNCLButtonDown);'
          'begin'
          '  inherited;'
          '  //Self.Perform(WM_VSCROLL, '
          'SB_LINEDOWN, 0);'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WMNCPAINT'
          '(var msgr: TWMNCPaint);'
          'begin'
          '  //inherited;'
          '  Self.PaintNC;'
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WndProc(var '
          'msgr: TMessage);'
          'var'
          '  vsi1, hsi1, vsi2, hsi2: TScrollInfo;'
          '  style: Integer;'
          '  changed: Boolean;'
          'begin'
          '  if not HandleAllocated or (msgr.Msg = '
          'WM_CREATE) or (msgr.Msg = '
          'WM_NCCREATE)'
          '    or (msgr.Msg = WM_DESTROY) or '
          '(msgr.Msg = WM_NCDESTROY) then'
          '  begin'
          '    inherited;'
          '    Exit;'
          '  end;'
          ''
          '  changed := False;'
          ''
          '  style := GetWindowLong(Self.Handle, '
          'GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi1.cbSize := SizeOf(vsi1);'
          '    vsi1.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi1);'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi1.cbSize := SizeOf(hsi1);'
          '    hsi1.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi1);'
          '  end;'
          '    '
          '  inherited;'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi2.cbSize := SizeOf(vsi2);'
          '    vsi2.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi2);'
          ''
          '    if (vsi1.nMin <> vsi2.nMin) or (vsi1.nMax '
          '<> vsi2.nMax)'
          '      or (vsi1.nPage <> vsi2.nPage) or '
          '(vsi1.nPos <> vsi2.nPos) then'
          '      changed := True;'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi2.cbSize := SizeOf(hsi2);'
          '    hsi2.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi2);'
          ''
          '    if (hsi1.nMin <> hsi2.nMin) or (hsi1.nMax '
          '<> hsi2.nMax)'
          '      or (hsi1.nPage <> hsi2.nPage) or '
          '(hsi1.nPos <> hsi2.nPos) then'
          '      changed := True;'
          '  end;'
          ''
          '  if changed then Self.PaintNC;'
          'end;'
          ''
          '{ TFsGraphicControl }'
          ''
          'procedure '
          'TFsGraphicControl.AutoSizeAndInvalidate;'
          'begin'
          '  if not CheckAutoSize then Self.Invalidate;'
          'end;'
          ''
          'function TFsGraphicControl.CanAutoSize(var '
          'NewWidth, NewHeight: Integer): Boolean;'
          'var'
          '  dim: TSize;'
          'begin'
          '  Result := True;'
          ''
          '  if not (csDestroying in ComponentState) '
          'then'
          '  begin'
          '    Self.GetContentDimension(dim);'
          ''
          '    if (csDesigning in ComponentState) or '
          '(dim.cx > 0) and (dim.cy > 0) then'
          '    begin'
          '      if Align in [alNone, alLeft, alRight] then '
          'NewWidth := dim.cx;'
          '      if Align in [alNone, alTop, alBottom] then '
          'NewHeight := dim.cy;'
          '    end;'
          '  end;'
          'end;'
          ''
          'function TFsGraphicControl.CheckAutoSize: '
          'Boolean;'
          'var'
          '  OldBounds: TRect;'
          'begin'
          '  if Self.AutoSize then'
          '  begin'
          '    OldBounds := Self.BoundsRect;'
          '    '
          '    Self.AutoSize := False;'
          '    Self.AutoSize := True;'
          ''
          '    Result := (OldBounds.Left <> Self.Left) or '
          '(OldBounds.Top <> Self.Top)'
          '      or (OldBounds.Right <> Self.Left + '
          'Self.Width)'
          '      or (OldBounds.Bottom <> Self.Top + '
          'Self.Height);'
          '    '
          '  end'
          '  else Result := False;'
          'end;'
          ''
          'procedure '
          'TFsGraphicControl.GetContentDimension'
          '(out dim: TSize);'
          'begin'
          '  dim.cx := Self.Width;'
          '  dim.cy := Self.Height;'
          'end;'
          ''
          '{ TFsCoverButton }'
          ''
          'constructor TFsCoverButton.Create'
          '(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FPicture := TPicture.Create;'
          '  FPicture.OnChange := '
          'Self.PictureChanged;'
          'end;'
          ''
          'destructor TFsCoverButton.Destroy;'
          'begin'
          '  FPicture.Free;'
          '  SetDownCover(nil);'
          '  SetHoverCover(nil);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsCoverButton.DrawPicture'
          '(const Rect: TRect);'
          'begin'
          '  if Assigned(FPicture.Graphic) and not '
          'FPicture.Graphic.Empty then'
          '    Self.Canvas.StretchDraw(Rect, '
          'FPicture.Graphic);'
          'end;'
          ''
          'function TFsCoverButton.GetCover: '
          'TFsDrawable;'
          'begin'
          '  inherited;'
          ''
          '  if mfLButtonDown in FMouseFlag then '
          'Result := fDownCover'
          '  else if mfMouseOver in FMouseFlag then '
          'Result := FHoverCover'
          '  else Result := nil;'
          'end;'
          ''
          'function TFsCoverButton.GetPictureSize(out '
          'width: Integer): Integer;'
          'begin'
          '  if Assigned(FPicture.Graphic) and not '
          'FPicture.Graphic.Empty then'
          '  begin'
          '    width := FPicture.Graphic.Width;'
          '    Result := FPicture.Graphic.Height;'
          '  end'
          '  else begin'
          '    width := 0;'
          '    Result := 0;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCoverButton.LinkedPictureChanged'
          '(Sender: TObject; ID: TNotifyID);'
          'var'
          '  changed: Boolean;'
          'begin'
          '  changed := (Sender = fDownCover) or '
          '(Sender = FHoverCover);'
          ''
          '  if ID = niDestroy then'
          '  begin'
          '    if Sender = fDownCover then '
          'fDownCover := nil;'
          ''
          '    if Sender = FHoverCover then '
          'FHoverCover := nil;'
          '  end;'
          ''
          '  if changed then Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCoverButton.Paint;'
          'var'
          '  cover: TFsDrawable;'
          '  dim: TSize;'
          '  w, h: Integer;'
          '  r: TRect;'
          'begin'
          '  inherited;'
          ''
          '  Self.GetContentDimension(dim);'
          '  cover := Self.GetCover;'
          ''
          '  if Assigned(cover) then'
          '  begin'
          '    w := cover.Width;'
          ''
          '    if (w < dim.cx) and cover.HorzSafeStretch '
          'then w := dim.cx;'
          ''
          '    h := cover.Height;'
          ''
          '    if (h < dim.cy) and cover.VertSafeStretch '
          'then h := dim.cy;'
          ''
          '    r.Left := (Self.Width - w) div 2;'
          '    r.Right := r.Left + w;'
          '    r.Top := (Self.Height - h) div 2;'
          '    r.Bottom := r.Top + h;'
          ''
          '    if cover is TFsSingleDrawable then'
          '      TFsSingleDrawable(cover).Draw'
          '(Self.Canvas, r)'
          '    else if cover is TFsMultiFrameDrawable '
          'then'
          '      TFsMultiFrameDrawable'
          '(cover).DrawFrame(Self.Canvas.Handle, r, '
          '0);'
          '  end;'
          'end;'
          ''
          'procedure TFsCoverButton.PictureChanged'
          '(Sender: TObject);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCoverButton.SetDownCover'
          '(const Value: TFsDrawable);'
          'begin'
          '  if fDownCover <> Value then'
          '  begin'
          '    if Assigned(fDownCover) then'
          '      fDownCover.RemoveOnChangeListener'
          '(Self.LinkedPictureChanged);'
          ''
          '    fDownCover := Value;'
          ''
          '    if Assigned(fDownCover) then'
          '      fDownCover.AddOnChangeListener'
          '(Self.LinkedPictureChanged);'
          ''
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCoverButton.SetHoverCover'
          '(const Value: TFsDrawable);'
          'begin'
          '  if FHoverCover <> Value then'
          '  begin'
          '    if Assigned(FHoverCover) then'
          '      '
          'FHoverCover.RemoveOnChangeListener'
          '(Self.LinkedPictureChanged);'
          ''
          '    FHoverCover := Value;'
          ''
          '    if Assigned(FHoverCover) then'
          '      FHoverCover.AddOnChangeListener'
          '(Self.LinkedPictureChanged);'
          ''
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCoverButton.SetPicture(const '
          'Value: TPicture);'
          'begin'
          '  FPicture.Assign(Value);'
          'end;'
          ''
          '{ TFsCustomScrollBar }'
          ''
          'const'
          '  SCROLL_MIN_THUMB_LENGTH = 20;'
          ''
          'function TFsCustomScrollBar.CalcHPos(const '
          'rc: TRect; const si: TScrollInfo; Y: Integer): '
          'Integer;'
          'var'
          '  ThumbWidth, ScrollWidth: Integer;'
          'begin'
          '  if (si.nMax >= si.nMin) and (si.nPage <= '
          'si.nMax - si.nMin) then'
          '  begin'
          '    ScrollWidth := rc.Right - rc.Left - '
          'FHArrowWidth shl 1;'
          ''
          '    ThumbWidth := si.nPage * ScrollWidth '
          'div (si.nMax - si.nMin + 1);'
          ''
          '    if ThumbWidth < Self.MinThumbLength '
          'then ThumbWidth := Self.MinThumbLength;'
          ''
          '    if Y <= rc.Left + FHArrowWidth then '
          'Result := si.nMin'
          '    else if Y > rc.Right - FHArrowWidth - '
          'ThumbWidth then Result := si.nMax'
          '    else begin'
          '      Y := Y - rc.Left - FHArrowWidth;'
          '      Result := (si.nMax - si.nMin + 1 - '
          'si.nPage) * Y div (ScrollWidth - '
          'ThumbWidth);'
          '    end;'
          '  end'
          '  else Result := si.nMin - 1;'
          'end;'
          ''
          'procedure TFsCustomScrollBar.CalcHScroll'
          '(const rc: TRect; const si: TScrollInfo; var '
          'rcLeftArrow, rcRightArrow,'
          '  rcThumb: TRect);'
          'var'
          '  ThumbWidth, ThumbPos, ScrollWidth: '
          'Integer;'
          'begin'
          '  rcLeftArrow.Left := rc.Left;'
          '  rcLeftArrow.Right := rc.Left + '
          'FHArrowWidth;'
          '  rcLeftArrow.Top := rc.Top;'
          '  rcLeftArrow.Bottom := rc.Bottom;'
          ''
          '  rcRightArrow.Left := rc.Right - '
          'FHArrowWidth;'
          '  rcRightArrow.Right := rc.Right;'
          '  rcRightArrow.Top := rc.Top;'
          '  rcRightArrow.Bottom := rc.Bottom;'
          ''
          '  if (si.nMax >= si.nMin) and (si.nPage <= '
          'si.nMax - si.nMin) then'
          '  begin'
          '    ScrollWidth := rc.Right - rc.Left - '
          'FHArrowWidth shl 1;'
          ''
          '    rcThumb.Top := rc.Top;'
          '    rcThumb.Bottom := rc.Bottom;'
          ''
          '    ThumbWidth := si.nPage * ScrollWidth '
          'div (si.nMax - si.nMin + 1);'
          ''
          '    if ThumbWidth < Self.MinThumbLength '
          'then ThumbWidth := Self.MinThumbLength;'
          ''
          '    ThumbPos := si.nPos * (ScrollWidth - '
          'ThumbWidth) div (si.nMax - si.nMin + 1 - '
          'si.nPage);'
          ''
          '    rcThumb.Left := rc.Left + FHArrowWidth '
          '+ ThumbPos;'
          '    rcThumb.Right := rcThumb.Left + '
          'ThumbWidth;'
          '  end'
          '  else begin'
          '    rcThumb.Left := 0;'
          '    rcThumb.Right := -1;'
          '    rcThumb.Top := 0;'
          '    rcThumb.Bottom := -1;'
          '  end;'
          'end;'
          ''
          'function TFsCustomScrollBar.CalcVPos(const '
          'rc: TRect; const si: TScrollInfo; Y: Integer): '
          'Integer;'
          'var'
          '  ThumbHeight, ScrollHeight: Integer;'
          'begin'
          '  if (si.nMax >= si.nMin) and (si.nPage <= '
          'si.nMax - si.nMin) then'
          '  begin'
          '    ScrollHeight := rc.Bottom - rc.Top - '
          'FVArrowHeight shl 1;'
          ''
          '    ThumbHeight := si.nPage * ScrollHeight '
          'div (si.nMax - si.nMin + 1);'
          ''
          '    if ThumbHeight < Self.MinThumbLength '
          'then ThumbHeight := '
          'Self.MinThumbLength;'
          ''
          '    if Y <= rc.Top + FVArrowHeight then '
          'Result := si.nMin'
          '    else if Y > rc.Bottom - FVArrowHeight - '
          'ThumbHeight then Result := si.nMax'
          '    else begin'
          '      Y := Y - rc.Top - FVArrowHeight;'
          '      Result := (si.nMax - si.nMin + 1 - '
          'si.nPage) * Y div (ScrollHeight - '
          'ThumbHeight);'
          '    end;'
          '  end'
          '  else Result := si.nMin - 1;'
          'end;'
          ''
          'procedure TFsCustomScrollBar.CalcVScroll'
          '(const rc: TRect; const si: TScrollInfo;'
          '  var rcTopArrow, rcBottomArrow, '
          'rcThumb: TRect);'
          'var'
          '  ThumbHeight, ThumbPos, ScrollHeight: '
          'Integer;'
          'begin'
          '  rcTopArrow.Left := rc.Left;'
          '  rcTopArrow.Right := rc.Right;'
          '  rcTopArrow.Top := rc.Top;'
          '  rcTopArrow.Bottom := rcTopArrow.Top + '
          'FVArrowHeight;'
          ''
          '  rcBottomArrow.Left := rc.Left;'
          '  rcBottomArrow.Right := rc.Right;'
          '  rcBottomArrow.Top := rc.Bottom - '
          'FVArrowHeight;'
          '  rcBottomArrow.Bottom := rc.Bottom;'
          ''
          '  if (si.nMax >= si.nMin) and (si.nPage <= '
          'si.nMax - si.nMin) then'
          '  begin'
          '    ScrollHeight := rc.Bottom - rc.Top - '
          'FVArrowHeight shl 1;'
          '    '
          '    rcThumb.Left := rc.Left;'
          '    rcThumb.Right := rc.Right;'
          ''
          '    ThumbHeight := si.nPage * ScrollHeight '
          'div (si.nMax - si.nMin + 1);'
          ''
          '    if ThumbHeight < Self.MinThumbLength '
          'then ThumbHeight := '
          'Self.MinThumbLength;'
          '    '
          '    ThumbPos := si.nPos * (ScrollHeight - '
          'ThumbHeight) div (si.nMax - si.nMin + 1 - '
          'si.nPage);'
          ''
          '    rcThumb.Top := rc.Top + FVArrowHeight '
          '+ ThumbPos;'
          '    rcThumb.Bottom := rcThumb.Top + '
          'ThumbHeight;'
          '  end'
          '  else begin'
          '    rcThumb.Left := 0;'
          '    rcThumb.Right := -1;'
          '    rcThumb.Top := 0;'
          '    rcThumb.Bottom := -1;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomScrollBar.Changed;'
          'begin'
          '  '
          'end;'
          ''
          'constructor TFsCustomScrollBar.Create'
          '(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FVScrollWidth := GetSystemMetrics'
          '(SM_CXVSCROLL);'
          '  FHScrollHeight := GetSystemMetrics'
          '(SM_CYHSCROLL);'
          '  FVArrowHeight := GetSystemMetrics'
          '(SM_CYVSCROLL);'
          '  FHArrowWidth := GetSystemMetrics'
          '(SM_CXHSCROLL);'
          '  FMinThumbLength := '
          'SCROLL_MIN_THUMB_LENGTH;'
          'end;'
          ''
          'procedure '
          'TFsCustomScrollBar.SetHArrowWidth(const '
          'Value: Integer);'
          'begin'
          '  if (FHArrowWidth <> Value) and (Value > '
          '0) then'
          '  begin'
          '    FHArrowWidth := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomScrollBar.SetHScrollHeight(const '
          'Value: Integer);'
          'begin'
          '  if (FHScrollHeight <> Value) and (Value > 0) '
          'then'
          '  begin'
          '    FHScrollHeight := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomScrollBar.SetMinThumbLength'
          '(const Value: Integer);'
          'begin'
          '  if (FMinThumbLength <> Value) and (Value '
          '>= SCROLL_MIN_THUMB_LENGTH) then'
          '  begin'
          '    FMinThumbLength := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomScrollBar.SetVArrowHeight(const '
          'Value: Integer);'
          'begin'
          '  if (FVArrowHeight <> Value) and (Value > '
          '0) then'
          '  begin'
          '    FVArrowHeight := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsCustomScrollBar.SetVScrollWidth(const '
          'Value: Integer);'
          'begin'
          '  if (FVScrollWidth <> Value) and (Value > 0) '
          'then'
          '  begin'
          '    FVScrollWidth := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          '{ TFsFlatScrollBar }'
          ''
          'procedure TFsFlatScrollBar.DrawHScroll(dc: '
          'HDC; const rc, rcLeftArrow, rcRightArrow, '
          'rcThumb: TRect);'
          'var'
          '  brush: HBRUSH;'
          'begin'
          '  Windows.FillRect(dc, rc, GetStockObject'
          '(LTGRAY_BRUSH));'
          ''
          '  GradientFillRect(dc, Rect(rcLeftArrow.Left + '
          '1, rcLeftArrow.Top + 1,'
          '    rcLeftArrow.Right - 1, rcLeftArrow.Bottom '
          '- 1),'
          '    RGB(255, 255, 255), RGB(229, 229, '
          '229), False);'
          ''
          '  Self.DrawLeftArrow(dc, rcLeftArrow,'
          '    (rcLeftArrow.Right - rcLeftArrow.Left) div '
          '2,'
          '    (rcLeftArrow.Bottom - rcLeftArrow.Top) '
          'div 2);'
          ''
          '  GradientFillRect(dc, Rect(rcRightArrow.Left '
          '+ 1, rcRightArrow.Top + 1,'
          '    rcRightArrow.Right - 1, '
          'rcRightArrow.Bottom - 1),'
          '    RGB(255, 255, 255), RGB(229, 229, '
          '229), False);'
          ''
          '  Self.DrawRightArrow(dc, rcRightArrow,'
          '    (rcRightArrow.Right - rcRightArrow.Left) '
          'div 2,'
          '    (rcRightArrow.Bottom - '
          'rcRightArrow.Top) div 2);'
          ''
          '  if rcThumb.Left < rcThumb.Right then'
          '  begin'
          '    brush := CreateSolidBrush(RGB(136, 136, '
          '136));'
          ''
          '    FillRect(dc, Rect(rcThumb.Left, '
          'rcThumb.Top + 1, rcThumb.Right - 1, '
          'rcThumb.Top + 2), GetStockObject'
          '(WHITE_BRUSH));'
          '    FillRect(dc, Rect(rcThumb.Left, '
          'rcThumb.Top + 2, rcThumb.Left + 1, '
          'rcThumb.Bottom - 1), GetStockObject'
          '(WHITE_BRUSH));'
          '    FillRect(dc, Rect(rcThumb.Right - 1, '
          'rcThumb.Top + 1, rcThumb.Right, '
          'rcThumb.Bottom - 1), brush);'
          '    FillRect(dc, Rect(rcThumb.Left + 1, '
          'rcThumb.Bottom - 2, rcThumb.Right - 1, '
          'rcThumb.Bottom - 1), brush);'
          ''
          '    DeleteObject(brush);'
          ''
          '    GradientFillRect(dc, Rect(rcThumb.Left + '
          '1, rcThumb.Top + 2, rcThumb.Right - 1, '
          'rcThumb.Bottom - 2),'
          '      RGB(254, 254, 254), RGB(229, 229, '
          '229), True);'
          '  end;'
          'end;'
          ''
          'procedure TFsFlatScrollBar.DrawVScroll(dc: '
          'HDC; const rc, rcTopArrow, rcBottomArrow, '
          'rcThumb: TRect);'
          'var'
          '  brush: HBRUSH;'
          'begin'
          '  Windows.FillRect(dc, rc, GetStockObject'
          '(LTGRAY_BRUSH));'
          ''
          '  GradientFillRect(dc, Rect(rcTopArrow.Left + '
          '1, rcTopArrow.Top + 1,'
          '    rcTopArrow.Right - 1, '
          'rcTopArrow.Bottom - 1),'
          '    RGB(255, 255, 255), RGB(229, 229, '
          '229), False);'
          ''
          '  Self.DrawUpArrow(dc, rcTopArrow, '
          '(rcTopArrow.Bottom - rcTopArrow.Top) div '
          '2, (rcTopArrow.Right - '
          'rcTopArrow.Left) div 2);'
          ''
          '  GradientFillRect(dc, Rect'
          '(rcBottomArrow.Left + 1, '
          'rcBottomArrow.Top + 1,'
          '    rcBottomArrow.Right - 1, '
          'rcBottomArrow.Bottom - 1),'
          '    RGB(255, 255, 255), RGB(229, 229, '
          '229), False);'
          ''
          '  Self.DrawDownArrow(dc, rcBottomArrow, '
          '(rcBottomArrow.Bottom - '
          'rcBottomArrow.Top) div 2, '
          '(rcBottomArrow.Right - rcBottomArrow.Left) '
          'div 2);'
          ''
          '  if rcThumb.Left < rcThumb.Right then'
          '  begin'
          '    brush := CreateSolidBrush(RGB(136, 136, '
          '136));'
          ''
          '    FillRect(dc, Rect(rcThumb.Left + 1, '
          'rcThumb.Top, rcThumb.Right - 2, '
          'rcThumb.Top + 1), GetStockObject'
          '(WHITE_BRUSH));'
          '    FillRect(dc, Rect(rcThumb.Left + 1, '
          'rcThumb.Top + 1, rcThumb.Left + 2, '
          'rcThumb.Bottom), GetStockObject'
          '(WHITE_BRUSH));'
          '    FillRect(dc, Rect(rcThumb.Right - 2, '
          'rcThumb.Top, rcThumb.Right - 1, '
          'rcThumb.Bottom), brush);'
          '    FillRect(dc, Rect(rcThumb.Left + 2, '
          'rcThumb.Bottom - 1, rcThumb.Right - 1, '
          'rcThumb.Bottom), brush);'
          ''
          '    DeleteObject(brush);'
          ''
          '    GradientFillRect(dc, Rect(rcThumb.Left + '
          '2, rcThumb.Top + 1, rcThumb.Right - 2, '
          'rcThumb.Bottom - 1),'
          '      RGB(254, 254, 254), RGB(229, 229, '
          '229), False);'
          '  end;'
          'end;'
          ''
          'procedure TFsFlatScrollBar.DrawDownArrow'
          '(dc: HDC; const rc: TRect; h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[0].X := rc.Left + (rc.Right - rc.Left - '
          'bw) div 2 ;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top - '
          'h) div 2;'
          ''
          '  pts[1].X := pts[0].X + bw;'
          '  pts[1].Y := pts[0].Y;'
          ''
          '  pts[2].X := rc.Left + (rc.Right - rc.Left) div '
          '2;'
          '  pts[2].Y := pts[0].Y + h;'
          ''
          '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, '
          'pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,'
          '    RGB(180, 180, 180), RGB(180, 180, '
          '180), RGB(192, 192, 192));'
          'end;'
          ''
          'procedure TFsFlatScrollBar.DrawIntersect'
          '(dc: HDC; const rc: TRect);'
          'begin'
          '  FillRect(dc, rc, GetStockObject'
          '(LTGRAY_BRUSH));'
          'end;'
          ''
          'procedure TFsFlatScrollBar.DrawLeftArrow'
          '(dc: HDC; const rc: TRect; h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[0].X := rc.Left + (rc.Right - rc.Left - h) '
          'div 2;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top) '
          'div 2;'
          ''
          '  pts[1].X := pts[0].X + h;'
          '  pts[1].Y := rc.Top + (rc.Bottom - rc.Top - '
          'bw) div 2;'
          ''
          '  pts[2].X := pts[1].X;'
          '  pts[2].Y := pts[1].Y + bw;'
          ''
          '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, '
          'pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,'
          '    RGB(192, 192, 192), RGB(180, 180, '
          '180), RGB(180, 180, 180));'
          'end;'
          ''
          'procedure TFsFlatScrollBar.DrawRightArrow'
          '(dc: HDC; const rc: TRect; h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[1].X := rc.Left + (rc.Right - rc.Left - h) '
          'div 2;'
          '  pts[1].Y := rc.Top + (rc.Bottom - rc.Top - '
          'bw) div 2;'
          ''
          '  pts[2].X := pts[1].X;'
          '  pts[2].Y := pts[1].Y + bw;'
          ''
          '  pts[0].X := pts[1].X + h;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top) '
          'div 2;'
          ''
          '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, '
          'pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,'
          '    RGB(192, 192, 192), RGB(180, 180, '
          '180), RGB(180, 180, 180));'
          'end;'
          ''
          'procedure TFsFlatScrollBar.DrawUpArrow'
          '(dc: HDC; const rc: TRect; h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[0].X := rc.Left + (rc.Right - rc.Left) div '
          '2;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top - '
          'h) div 2;'
          ''
          '  pts[1].X := rc.Left + (rc.Right - rc.Left - '
          'bw) div 2;'
          '  pts[1].Y := pts[0].Y + h;'
          ''
          '  pts[2].X := pts[1].X + bw;'
          '  pts[2].Y := pts[1].Y;'
          ''
          '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, '
          'pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,'
          '    RGB(192, 192, 192), RGB(180, 180, '
          '180), RGB(180, 180, 180));'
          'end;'
          ''
          '{ TFsScrollContainer }'
          ''
          'procedure '
          'TFsScrollContainer.AdjustInnerControlBound'
          's;'
          'var'
          '  L, R, T, B: Integer;'
          '  sb: TFsCustomScrollBar;'
          '  VScroll, HScroll: Boolean;'
          'begin'
          '  if Assigned(FRealControl) then'
          '  begin'
          '  L := 2;'
          '    R := Self.ClientWidth - 2;'
          '    T := 2;'
          '    B := Self.ClientHeight - 2;'
          ''
          '    VScroll := Self.NeedScrollBar(HScroll);'
          ''
          '    sb := Self.GetRealScrollBar;'
          ''
          '    if VScroll then Dec(R, sb.VScrollWidth);'
          ''
          '    if HScroll then Dec(B, sb.HScrollHeight);'
          ''
          '    FRealControl.SetBounds(L, T, R - L, B - '
          'T);'
          '  end;'
          'end;'
          ''
          'procedure '
          'TFsScrollContainer.CMMouseEnter(var msgr: '
          'TMessage);'
          'begin'
          '  inherited;'
          '  FMouseInControl := True;'
          '  Self.Invalidate;'
          'end;'
          ''
          'procedure '
          'TFsScrollContainer.CMMouseLeave(var '
          'msgr: TMessage);'
          'begin'
          '  inherited;'
          '  FMouseInControl := False;'
          '  Self.Invalidate;'
          'end;'
          ''
          'function TFsScrollContainer.ControlMessage'
          '(msg: DWORD; wparam, lparam: Integer): '
          'Integer;'
          'begin'
          '  if Assigned(FRealControl) then Result := '
          'FRealControl.Perform(msg, wparam, lparam)'
          '  else Result := -1;'
          'end;'
          ''
          'constructor TFsScrollContainer.Create'
          '(AOwner: TComponent);'
          'begin'
          '  inherited;'
          ''
          '  ControlStyle := [csAcceptsControls, '
          'csCaptureMouse, csClickEvents,'
          '    csSetCaption, csOpaque, csDoubleClicks, '
          'csReplicatable, csPannable];'
          ''
          '  Width := 185;'
          '  Height := 41;'
          ''
          '  FRealControl := Self.CreateRealControl;'
          '  FRealControl.Parent := Self;'
          ''
          '  FTimer := TTimer.Create(Self);'
          '  FTimer.Enabled := False;'
          '  FTimer.Interval := 200;'
          '  FTimer.OnTimer := Self.OnTimer;'
          'end;'
          ''
          'procedure TFsScrollContainer.CreateParams'
          '(var Params: TCreateParams);'
          'begin'
          '  inherited;'
          ''
          '  Params.Style := Params.Style and not '
          'WS_BORDER;'
          '  Params.WindowClass.style := '
          'Params.WindowClass.style or CS_HREDRAW '
          'or CS_VREDRAW;'
          'end;'
          ''
          'destructor TFsScrollContainer.Destroy;'
          'begin'
          ''
          '  inherited;'
          'end;'
          ''
          'function '
          'TFsScrollContainer.GetControlScrollInfo(var '
          'si: TScrollInfo; isVert: Boolean): Boolean;'
          'const'
          '  BARS: array [Boolean] of DWORD = '
          '(SB_HORZ, SB_VERT);'
          'begin'
          '  if Assigned(FRealControl) and (FRealControl '
          'is TWinControl) and (TWinControl'
          '(FRealControl).HandleAllocated) then'
          '    Result := Windows.GetScrollInfo'
          '(TWinControl(FRealControl).Handle, BARS'
          '[isVert], si) and (si.nPage <> 0)'
          '  else Result := False;'
          'end;'
          ''
          'function TFsScrollContainer.GetRealScrollBar: '
          'TFsCustomScrollBar;'
          'begin'
          '  if Assigned(FScrollBarDrawer) then Result '
          ':= FScrollBarDrawer'
          '  else Result := GetDefaultScrollBar;'
          'end;'
          ''
          'procedure TFsScrollContainer.GetScrollInfo'
          '(var fsi: TFsScrollInfo; var vsi, hsi: '
          'TScrollInfo);'
          'var'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  sb := GetRealScrollBar;'
          ''
          '  fsi.ShowVScroll := False;'
          '  fsi.ShowHScroll := False;'
          ''
          '  vsi.cbSize := SizeOf(vsi);'
          '  vsi.fMask := SIF_RANGE or SIF_POS or '
          'SIF_PAGE;'
          ''
          '  if GetControlScrollInfo(vsi, True) and '
          '(vsi.nMax - vsi.nMin + 1 > vsi.nPage) then'
          '    fsi.ShowVScroll := True;'
          ''
          '  hsi.cbSize := SizeOf(hsi);'
          '  hsi.fMask := SIF_RANGE or SIF_POS or '
          'SIF_PAGE;'
          ''
          '  if GetControlScrollInfo(hsi, False) and '
          '(hsi.nMax - hsi.nMin + 1 > hsi.nPage) then'
          '    fsi.ShowHScroll := True;'
          ''
          '  if fsi.ShowVScroll then'
          '  begin'
          '    fsi.VScroll.Right := Self.Width - 2;'
          '    fsi.VScroll.Left := fsi.VScroll.Right - '
          'sb.VScrollWidth;'
          '    fsi.VScroll.Top := 2;'
          ''
          '    if fsi.ShowHScroll then fsi.VScroll.Bottom '
          ':= Self.Height - 2 - sb.HScrollHeight'
          '    else fsi.VScroll.Bottom := Self.Height - 2;'
          ''
          '    sb.CalcVScroll(fsi.VScroll, vsi, fsi.TopArrow, '
          'fsi.BottomArrow, fsi.VThumb);'
          '  end;'
          ''
          '  if fsi.ShowHScroll then'
          '  begin'
          '    fsi.HScroll.Bottom := Self.Height - 2;'
          '    fsi.HScroll.Top := fsi.HScroll.Bottom - '
          'sb.HScrollHeight;'
          '    fsi.HScroll.Left := 2;'
          ''
          '    if fsi.ShowHScroll then fsi.HScroll.Right := '
          'Self.Width - 2 - sb.VScrollWidth'
          '    else fsi.HScroll.Right := Self.Width - 2;'
          ''
          '    sb.CalcHScroll(fsi.HScroll, hsi, fsi.LeftArrow, '
          'fsi.RightArrow, fsi.HThumb);'
          '  end;'
          ''
          '  if fsi.ShowVScroll and fsi.ShowHScroll then'
          '  begin'
          '    fsi.Intersect.Left := Self.Width - 2 - '
          'sb.VScrollWidth;'
          '    fsi.Intersect.Right := Self.Width - 2;'
          '    fsi.Intersect.Top := Self.Height - 2 - '
          'sb.HScrollHeight;'
          '    fsi.Intersect.Bottom := Self.Height - 2;'
          '  end;'
          'end;'
          ''
          'function TFsScrollContainer.HitTest(X, Y: '
          'Integer): TScrollHitTest;'
          'var'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          '  pt: TPoint;'
          'begin'
          '  pt.X := X;'
          '  pt.Y := Y;'
          ''
          '  Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '  Result := shtNoWhere;'
          ''
          '  if fsi.ShowVScroll then'
          '  begin'
          '    if PtInRect(fsi.TopArrow, pt) then Result '
          ':= shtTopArrow'
          '    else if PtInRect(fsi.BottomArrow, pt) then '
          'Result := shtBottomArrow'
          '    else if PtInRect(fsi.VThumb, pt) then '
          'Result := shtVertThumb'
          '    else if PtInRect(fsi.VScroll, pt) then'
          '    begin'
          '      if pt.Y < fsi.VThumb.Top then Result := '
          'shtPageUp'
          '      else Result := shtPageDown;'
          '    end;'
          '  end;'
          ''
          '  if fsi.ShowHScroll and (Result = '
          'shtNoWhere) then'
          '  begin'
          '    if PtInRect(fsi.LeftArrow, pt) then Result '
          ':= shtLeftArrow'
          '    else if PtInRect(fsi.RightArrow, pt) then '
          'Result := shtRightArrow'
          '    else if PtInRect(fsi.HThumb, pt) then '
          'Result := shtHorzThumb'
          '    else if PtInRect(fsi.HScroll, pt) then'
          '    begin'
          '      if pt.X < fsi.HThumb.Left then Result := '
          'shtPageLeft'
          '      else Result := shtPageRight;'
          '    end;'
          '  end;'
          'end;'
          ''
          'procedure TFsScrollContainer.MouseDown'
          '(Button: TMouseButton; Shift: TShiftState; '
          'X, Y: Integer);'
          'var'
          '  sht: TScrollHitTest;'
          '  DoCapture: Boolean;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          'begin'
          '  DoCapture := True;'
          '  sht := Self.HitTest(X, Y);'
          ''
          '  case sht of'
          '    shtNoWhere, shtBorder: DoCapture := '
          'False;'
          '    shtLeftArrow: Self.ControlMessage'
          '(WM_HSCROLL, SB_LINELEFT, 0);'
          '    shtRightArrow: Self.ControlMessage'
          '(WM_HSCROLL, SB_LINERIGHT, 0);'
          '    shtHorzThumb: ;'
          '    shtPageLeft: Self.ControlMessage'
          '(WM_HSCROLL, SB_PAGELEFT, 0);'
          '    shtPageRight: Self.ControlMessage'
          '(WM_HSCROLL, SB_PAGERIGHT, 0);'
          '    shtTopArrow: Self.ControlMessage'
          '(WM_VSCROLL, SB_LINEUP, 0);'
          '    shtBottomArrow: Self.ControlMessage'
          '(WM_VSCROLL, SB_LINEDOWN, 0);'
          '    shtVertThumb: ;'
          '    shtPageUp: Self.ControlMessage'
          '(WM_VSCROLL, SB_PAGEUP, 0);'
          '    shtPageDown: Self.ControlMessage'
          '(WM_VSCROLL, SB_PAGEDOWN, 0);'
          '  end;'
          ''
          '  if DoCapture then'
          '  begin'
          '    FCaptureRegion := sht;'
          '    SetCapture(Self.Handle);'
          ''
          '    if FCaptureRegion in [shtVertThumb, '
          'shtHorzThumb] then'
          '    begin'
          '      Self.GetScrollInfo(fsi, vsi, hsi);'
          '      FCapturePoint.X := X - fsi.HThumb.Left;'
          '      FCapturePoint.Y := Y - fsi.VThumb.Top;'
          '    end'
          '    else FTimer.Enabled := True;'
          '  end;'
          ''
          '  inherited;'
          'end;'
          ''
          'procedure TFsScrollContainer.MouseMove'
          '(Shift: TShiftState; X, Y: Integer);'
          'var'
          '  pt: TPoint;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          '  nPos: Integer;'
          'begin'
          '  case FCaptureRegion of'
          '    shtLeftArrow, shtRightArrow, shtPageLeft, '
          'shtPageRight,'
          '    shtTopArrow, shtBottomArrow, '
          'shtPageUp, shtPageDown:'
          '      begin'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          '        FTimer.Enabled := HitTest(pt.X, pt.Y) = '
          'FCaptureRegion;'
          '      end;'
          ''
          '    shtVertThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowVScroll then'
          '        begin'
          '          nPos := GetRealScrollBar.CalcVPos'
          '(fsi.VScroll, vsi, Y - FCapturePoint.Y);'
          ''
          '          if nPos >= hsi.nMin then'
          '            Self.ControlMessage(WM_VSCROLL, '
          'MakeLong(SB_THUMBTRACK, nPos),0);'
          '        end;'
          '      end;'
          ''
          '    shtHorzThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowHScroll then'
          '        begin'
          '          nPos := GetRealScrollBar.CalcHPos'
          '(fsi.HScroll, hsi, X - FCapturePoint.X);'
          ''
          '          if nPos >= hsi.nMin then'
          '            Self.ControlMessage(WM_HSCROLL, '
          'MakeLong(SB_THUMBTRACK, nPos),0);'
          '        end;'
          '      end;'
          '  end;'
          ''
          '  inherited;'
          'end;'
          ''
          'procedure TFsScrollContainer.MouseUp'
          '(Button: TMouseButton; Shift: TShiftState; '
          'X, Y: Integer);'
          'var'
          '  pt: TPoint;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          '  nPos: Integer;'
          'begin'
          '  ReleaseCapture;'
          '  FCaptureRegion := shtNoWhere;'
          '  FTimer.Enabled := False;'
          ''
          '  case FCaptureRegion of'
          '    shtVertThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowHScroll then'
          '        begin'
          '          nPos := GetRealScrollBar.CalcVPos'
          '(fsi.VScroll, vsi, Y - FCapturePoint.Y);'
          ''
          '          if nPos >= hsi.nMin then'
          '            Self.ControlMessage(WM_VSCROLL, '
          'MakeLong(SB_THUMBPOSITION, nPos),0);'
          '        end;'
          '      end;'
          ''
          '    shtHorzThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowHScroll then'
          '        begin'
          '          nPos := GetRealScrollBar.CalcHPos'
          '(fsi.HScroll, hsi, X - FCapturePoint.X);'
          ''
          '          if nPos >= hsi.nMin then'
          '            Self.ControlMessage(WM_HSCROLL, '
          'MakeLong(SB_THUMBPOSITION, nPos),0);'
          '        end;'
          '      end;'
          '  end;'
          '  '
          '  inherited;'
          'end;'
          ''
          'function TFsScrollContainer.NeedScrollBar'
          '(out HScroll: Boolean): Boolean;'
          'var'
          '  style: Integer;'
          '  si: TScrollInfo;'
          'begin'
          '  HScroll := False;'
          '  Result := False;'
          ''
          '  si.cbSize := SizeOf(si);'
          '  si.fMask := SIF_RANGE or SIF_PAGE;'
          ''
          '  HScroll := GetControlScrollInfo(si, False) '
          'and (si.nMax - si.nMin + 1 > si.nPage);'
          ''
          '  si.cbSize := SizeOf(si);'
          '  si.fMask := SIF_RANGE or SIF_PAGE;'
          ''
          '  Result := GetControlScrollInfo(si, True) and '
          '(si.nMax - si.nMin + 1 > si.nPage);'
          'end;'
          ''
          'procedure TFsScrollContainer.OnTimer'
          '(Sender: TObject);'
          'var'
          '  pt: TPoint;'
          'begin'
          '  case FCaptureRegion of'
          '    shtLeftArrow: Self.ControlMessage'
          '(WM_HSCROLL, SB_LINELEFT, 0);'
          '    shtRightArrow: Self.ControlMessage'
          '(WM_HSCROLL, SB_LINERIGHT, 0);'
          '    shtPageLeft:'
          '      begin'
          '        Self.ControlMessage(WM_HSCROLL, '
          'SB_PAGELEFT, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          '        if HitTest(pt.X, pt.Y) <> shtPageLeft '
          'then FTimer.Enabled := False;'
          '      end;'
          '    shtPageRight:'
          '      begin'
          '        Self.ControlMessage(WM_HSCROLL, '
          'SB_PAGERIGHT, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          '        if HitTest(pt.X, pt.Y) <> shtPageLeft '
          'then FTimer.Enabled := False;'
          '      end;'
          '    shtTopArrow: Self.ControlMessage'
          '(WM_VSCROLL, SB_LINEUP, 0);'
          '    shtBottomArrow: Self.ControlMessage'
          '(WM_VSCROLL, SB_LINEDOWN, 0);'
          '    shtPageUp:'
          '      begin'
          '        Self.ControlMessage(WM_VSCROLL, '
          'SB_PAGEUP, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          '        if HitTest(pt.X, pt.Y) <> shtPageUp '
          'then FTimer.Enabled := False;'
          '      end;'
          '    shtPageDown:'
          '      begin'
          '        Self.ControlMessage(WM_VSCROLL, '
          'SB_PAGEDOWN, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          '        if HitTest(pt.X, pt.Y) <> shtPageDown '
          'then FTimer.Enabled := False;'
          '      end;'
          '  end;'
          'end;'
          ''
          'procedure TFsScrollContainer.Paint;'
          'var'
          '  sb: TFsCustomScrollBar;'
          '  r: TRect;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          'begin'
          '  inherited;'
          ''
          '  if FMouseInControl then '
          'Canvas.Brush.Color := RGB(123, 228, 255)'
          '  else Canvas.Brush.Color := RGB(78, 160, '
          '209);'
          ''
          '  r.Left := 0;'
          '  r.Top := 0;'
          '  r.Right := Self.Width;'
          '  r.Bottom := Self.Height;'
          ''
          '  Canvas.FrameRect(r);'
          ''
          '  if FMouseInControl then '
          'Canvas.Brush.Color := RGB(78, 160, 209)'
          '  else Canvas.Brush.Color := TControlHack'
          '(FRealControl).Color;'
          ''
          '  r.Left := 1;'
          '  r.Top := 1;'
          '  r.Right := Self.Width - 1;'
          '  r.Bottom := Self.Height - 1;'
          ''
          '  Canvas.FrameRect(r);'
          ''
          '  sb := GetRealScrollBar;'
          ''
          '  Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '  if fsi.ShowVScroll then'
          '    sb.DrawVScroll(Canvas.Handle, fsi.VScroll, '
          'fsi.TopArrow, fsi.BottomArrow, fsi.VThumb);'
          ''
          '  if fsi.ShowHScroll then'
          '    sb.DrawHScroll(Canvas.Handle, fsi.HScroll, '
          'fsi.LeftArrow, fsi.RightArrow, fsi.HThumb);'
          ''
          '  if fsi.ShowVScroll and fsi.ShowHScroll then'
          '    sb.DrawIntersect(Canvas.Handle, '
          'fsi.Intersect);'
          'end;'
          ''
          'procedure '
          'TFsScrollContainer.SetScrollBarDrawer(const '
          'Value: TFsCustomScrollBar);'
          'begin'
          '  if Value <> FScrollBarDrawer then'
          '  begin'
          '    if Assigned(FScrollBarDrawer) then'
          '      '
          'FScrollBarDrawer.RemoveFreeNotification'
          '(Self);'
          ''
          '    FScrollBarDrawer := Value;'
          ''
          '    if Assigned(FScrollBarDrawer) then'
          '      FScrollBarDrawer.FreeNotification(Self);'
          ''
          '    Self.AdjustInnerControlBounds;'
          ''
          '    //Self.Invalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsScrollContainer.WMSize(var '
          'msgr: TWMSize);'
          'begin'
          '  inherited;'
          '  AdjustInnerControlBounds;'
          'end;'
          ''
          '{ TFsMemo }'
          ''
          'function TFsMemo.CreateRealControl: '
          'TControl;'
          'begin'
          '  Result := TFsBorderlessMemo.Create(Self);'
          '  TFsBorderlessMemo(Result).ScrollBars := '
          'ssVertical;'
          'end;'
          ''
          'function TFsMemo.GetLines: TStrings;'
          'begin'
          '  Result := TFsBorderlessMemo'
          '(RealControl).Lines;'
          'end;'
          ''
          'function TFsMemo.GetScrollBars: '
          'TScrollStyle;'
          'begin'
          '  Result := TFsBorderlessMemo'
          '(RealControl).ScrollBars;'
          'end;'
          ''
          'procedure TFsMemo.SetLines(const Value: '
          'TStrings);'
          'begin'
          '  TFsBorderlessMemo(RealControl).Lines := '
          'Value;'
          'end;'
          ''
          'procedure TFsMemo.SetScrollBars(const '
          'Value: TScrollStyle);'
          'begin'
          '  TFsBorderlessMemo(RealControl).ScrollBars '
          ':= Value;'
          'end;'
          ''
          '{ TFsBorderlessMemo }'
          ''
          'constructor TFsBorderlessMemo.Create'
          '(AOwner: TComponent);'
          'begin'
          '  inherited;'
          'end;'
          ''
          'procedure '
          'TFsBorderlessMemo.CreateParams(var '
          'params: TCreateParams);'
          'begin'
          '  inherited;'
          '  params.Style := params.Style and not '
          'WS_BORDER;'
          'end;'
          ''
          'procedure '
          'TFsBorderlessMemo.WMNCCalcSize(var '
          'msgr: TWMNCCalcSize);'
          'begin'
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsBorderlessMemo.WMNCPaint'
          '(var msgr: TWMNCPaint);'
          'begin'
          '  msgr.Result := 0;  '
          'end;'
          ''
          'procedure TFsBorderlessMemo.WndProc(var '
          'msgr: TMessage);'
          'var'
          '  vsi1, hsi1, vsi2, hsi2: TScrollInfo;'
          '  style: Integer;'
          '  changed: Boolean;'
          'begin'
          '  if not HandleAllocated or (msgr.Msg = '
          'WM_CREATE) or (msgr.Msg = '
          'WM_NCCREATE)'
          '    or (msgr.Msg = WM_DESTROY) or '
          '(msgr.Msg = WM_NCDESTROY) then'
          '  begin'
          '    inherited;'
          '    Exit;'
          '  end;'
          ''
          '  changed := False;'
          ''
          '  style := GetWindowLong(Self.Handle, '
          'GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi1.cbSize := SizeOf(vsi1);'
          '    vsi1.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi1);'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi1.cbSize := SizeOf(hsi1);'
          '    hsi1.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi1);'
          '  end;'
          '    '
          '  inherited;'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi2.cbSize := SizeOf(vsi2);'
          '    vsi2.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi2);'
          ''
          '    if (vsi1.nMin <> vsi2.nMin) or (vsi1.nMax '
          '<> vsi2.nMax)'
          '      or (vsi1.nPage <> vsi2.nPage) or '
          '(vsi1.nPos <> vsi2.nPos) then'
          '      changed := True;'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi2.cbSize := SizeOf(hsi2);'
          '    hsi2.fMask := SIF_RANGE or SIF_PAGE or '
          'SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi2);'
          ''
          '    if (hsi1.nMin <> hsi2.nMin) or (hsi1.nMax '
          '<> hsi2.nMax)'
          '      or (hsi1.nPage <> hsi2.nPage) or '
          '(hsi1.nPos <> hsi2.nPos) then'
          '      changed := True;'
          '  end;'
          ''
          '  if changed and (Self.Parent is TFsMemo) '
          'then'
          '  begin'
          '    TFsMemo'
          '(Self.Parent).AdjustInnerControlBounds;'
          '    Self.Parent.Invalidate;'
          '  end;'
          'end;'
          ''
          'initialization'
          ''
          'finalization'
          '  DefaultScrollBar.Free;'
          '  '
          'end.')
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'ListBox'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object FsListBox1: TFsListBox
        Left = 0
        Top = 0
        Width = 850
        Height = 509
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvRaised
        BevelKind = bkFlat
        ParentBackground = False
        Items.Strings = (
          'unit FSStdCtrls;'
          ''
          'interface'
          ''
          'uses'
          
            '  SysUtils, Classes, Consts, Windows, Graphics, Controls, Messag' +
            'es, StdCtrls, ExtCtrls,'
          '  FSVclBase, FSGraphics;'
          ''
          'type'
          '  TFsGraphicControl = class(TGraphicControl)'
          '  protected'
          '    procedure GetContentDimension(out dim: TSize); virtual;'
          
            '    function CanAutoSize(var NewWidth, NewHeight: Integer): Bool' +
            'ean; override;'
          '  public'
          '    function CheckAutoSize: Boolean;'
          '    procedure AutoSizeAndInvalidate;'
          '  end;'
          ''
          '  TFsImage = class(TFsGraphicControl)'
          '  private'
          '    FPicture: TFsDrawable;'
          '    procedure PictureChanged(Sender: TObject; ID: TNotifyID);'
          '    procedure SetPicture(const Value: TFsDrawable);'
          '  protected'
          '    procedure GetContentDimension(out dim: TSize); override;'
          '  public'
          '    constructor Create(Owner: TComponent); override;'
          '    destructor Destroy; override;'
          '    procedure Paint; override;'
          '  published'
          
            '    property Picture: TFsDrawable read FPicture write SetPicture' +
            ';'
          '    property Action;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSize;'
          '    property Enabled;'
          '    property ShowHint;'
          '    property Visible;'
          '    property OnClick;'
          '  end;'
          ''
          
            '  TFsImageButtonLayout = (blPictureLeft, blPictureRight, blPictu' +
            'reTop, blPictureBottom, blTextInPicture, blTextInStretchPicture)' +
            ';'
          ''
          '  TFsCustomButton = class(TFsGraphicControl)'
          '  private'
          '    FMouseFlag: TMouseFlag;'
          '    FShowCaption: Boolean;'
          '    FLayout: TFsImageButtonLayout;'
          '    FSpace: Integer;'
          '    procedure SetShowCaption(const Value: Boolean);'
          '    procedure SetLayout(const Value: TFsImageButtonLayout);'
          '    procedure SetSpace(const Value: Integer);'
          '    procedure DrawPictureLeft;'
          '    procedure DrawPictureRight;'
          '    procedure DrawPictureTop;'
          '    procedure DrawPictureBottom;'
          '    procedure DrawTextInPicture;'
          '    procedure DrawTextInStretchPicture;'
          '    procedure DrawButtonSurface;'
          '  protected'
          '    procedure DrawImageAndText;'
          '    procedure GetContentDimension(out dim: TSize); override;'
          
            '    procedure MouseDown(Button: TMouseButton; Shift: TShiftState' +
            '; X, Y: Integer); override;'
          
            '    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; ' +
            'X, Y: Integer); override;'
          
            '    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEE' +
            'NTER;'
          
            '    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSEL' +
            'EAVE;'
          
            '    procedure CMTextChange(var msg: TMessage); message CM_TEXTCH' +
            'ANGED;'
          
            '    procedure CMFontChanged(var msgr: TMessage); message CM_FONT' +
            'CHANGED;'
          '    function PictureIsHorzStretchable: Boolean; virtual;'
          '    function PictureIsVertStretchable: Boolean; virtual;'
          
            '    function GetPictureSize(out width: Integer): Integer; virtua' +
            'l; abstract;'
          '    procedure DrawPicture(const Rect: TRect); virtual; abstract;'
          '    procedure Paint; override;'
          '  public'
          '    constructor Create(Owner: TComponent); override;'
          '  published'
          
            '    property Layout: TFsImageButtonLayout read FLayout write Set' +
            'Layout default blTextInStretchPicture;'
          
            '    property Space: Integer read FSpace write SetSpace default 4' +
            ';'
          
            '    property ShowCaption: Boolean read FShowCaption write SetSho' +
            'wCaption default True;'
          '    property Action;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSize;'
          '    property Caption;'
          '    property Enabled;'
          '    property Font;'
          '    property ShowHint;'
          '    property Visible;'
          '    property OnClick;'
          '  end;'
          ''
          '  TFsImageButton = class(TFsCustomButton)'
          '  private'
          '    FPicture: TFsPictureDrawable;'
          '    FDisablePicture: TFsPictureDrawable;'
          '    FMouseOverPicture: TFsPictureDrawable;'
          '    FMouseDownPicture: TFsPictureDrawable;'
          '    procedure PictureChanged(Sender: TObject; ID: TNotifyID);'
          '    procedure SetPicture(const Value: TPicture);'
          '    procedure SetMouseDownPicture(const Value: TPicture);'
          '    procedure SetMouseOverPicture(const Value: TPicture);'
          '    procedure SetDisablePicture(const Value: TPicture);'
          '    function GetDisablePicture: TPicture;'
          '    function GetMouseDownPicture: TPicture;'
          '    function GetMouseOverPicture: TPicture;'
          '    function GetPicture: TPicture;'
          '    function GetDrawable: TFsSingleDrawable;'
          '  protected'
          
            '    function GetPictureSize(out width: Integer): Integer; overri' +
            'de;'
          '    procedure DrawPicture(const Rect: TRect); override;'
          '  public'
          '    constructor Create(Owner: TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          '    property Picture: TPicture read GetPicture write SetPicture;'
          
            '    property DisablePicture: TPicture read GetDisablePicture wri' +
            'te SetDisablePicture;'
          
            '    property MouseOverPicture: TPicture read GetMouseOverPicture' +
            ' write SetMouseOverPicture;'
          
            '    property MouseDownPicture: TPicture read GetMouseDownPicture' +
            ' write SetMouseDownPicture;'
          '  end;'
          ''
          '  TFsCoverButton = class(TFsCustomButton)'
          '  private'
          '    FPicture: TPicture;'
          '    FHoverCover: TFsDrawable;'
          '    fDownCover: TFsDrawable;'
          '    procedure SetPicture(const Value: TPicture);'
          '    procedure SetDownCover(const Value: TFsDrawable);'
          '    procedure SetHoverCover(const Value: TFsDrawable);'
          '    function GetCover: TFsDrawable;'
          '  protected'
          '    procedure PictureChanged(Sender: TObject);'
          
            '    procedure LinkedPictureChanged(Sender: TObject; ID: TNotifyI' +
            'D);'
          
            '    function GetPictureSize(out width: Integer): Integer; overri' +
            'de;'
          '    procedure DrawPicture(const Rect: TRect); override;'
          '    procedure Paint; override;'
          '  public'
          '    constructor Create(AOwner: TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          '    property Picture: TPicture read FPicture write SetPicture;'
          
            '    property HoverCover: TFsDrawable read FHoverCover write SetH' +
            'overCover;'
          
            '    property DownCover: TFsDrawable read fDownCover write SetDow' +
            'nCover;'
          '  end;'
          ''
          '  TFsSkinButton = class(TFsCustomButton)'
          '  '
          '  end;'
          ''
          '  TFsCustomScrollBar = class(TComponent)'
          '  private'
          '    FVScrollWidth: Integer;'
          '    FHScrollHeight: Integer;'
          '    FVArrowHeight: Integer;'
          '    FHArrowWidth: Integer;'
          '    FMinThumbLength: Integer;'
          '    procedure SetHScrollHeight(const Value: Integer);'
          '    procedure SetVScrollWidth(const Value: Integer);'
          '    procedure SetHArrowWidth(const Value: Integer);'
          '    procedure SetVArrowHeight(const Value: Integer);'
          '    procedure SetMinThumbLength(const Value: Integer);'
          '  protected'
          '    procedure Changed;'
          '  public'
          '    constructor Create(AOwner: TComponent); override;'
          
            '    procedure CalcVScroll(const rc: TRect; const si: TScrollInfo' +
            '; var rcTopArrow, rcBottomArrow, rcThumb: TRect);'
          
            '    procedure CalcHScroll(const rc: TRect; const si: TScrollInfo' +
            '; var rcLeftArrow, rcRightArrow, rcThumb: TRect);'
          
            '    function CalcVPos(const rc: TRect; const si: TScrollInfo; Y:' +
            ' Integer): Integer;'
          
            '    function CalcHPos(const rc: TRect; const si: TScrollInfo; Y:' +
            ' Integer): Integer;'
          
            '    procedure DrawVScroll(dc: HDC; const rc, rcTopArrow, rcBotto' +
            'mArrow, rcThumb: TRect); virtual; abstract;'
          
            '    procedure DrawHScroll(dc: HDC; const rc, rcLeftArrow, rcRigh' +
            'tArrow, rcThumb: TRect); virtual; abstract;'
          
            '    procedure DrawIntersect(dc: HDC; const rc: TRect); virtual; ' +
            'abstract;'
          
            '    property HScrollHeight: Integer read FHScrollHeight write Se' +
            'tHScrollHeight;'
          
            '    property VScrollWidth: Integer read FVScrollWidth write SetV' +
            'ScrollWidth;'
          
            '    property VArrowHeight: Integer read FVArrowHeight write SetV' +
            'ArrowHeight;'
          
            '    property HHArrowWidth: Integer read FHArrowWidth write SetHA' +
            'rrowWidth;'
          
            '    property MinThumbLength: Integer read FMinThumbLength write ' +
            'SetMinThumbLength;'
          '  end;'
          ''
          '  TFsFlatScrollBar = class(TFsCustomScrollBar)'
          '  private'
          
            '    procedure DrawUpArrow(dc: HDC; const rc: TRect; h, bw: Integ' +
            'er);'
          
            '    procedure DrawDownArrow(dc: HDC; const rc: TRect; h, bw: Int' +
            'eger);'
          
            '    procedure DrawLeftArrow(dc: HDC; const rc: TRect; h, bw: Int' +
            'eger);'
          
            '    procedure DrawRightArrow(dc: HDC; const rc: TRect; h, bw: In' +
            'teger);'
          '  public'
          
            '    procedure DrawVScroll(dc: HDC; const rc, rcTopArrow, rcBotto' +
            'mArrow, rcThumb: TRect); override;'
          
            '    procedure DrawHScroll(dc: HDC; const rc, rcLeftArrow, rcRigh' +
            'tArrow, rcThumb: TRect); override;'
          '    procedure DrawIntersect(dc: HDC; const rc: TRect); override;'
          '  end;'
          ''
          '  TFsScrollInfo = record'
          '    ShowVScroll: Boolean;'
          '    ShowHScroll: Boolean;'
          '    VScroll: TRect;'
          '    TopArrow: TRect;'
          '    BottomArrow: TRect;'
          '    VThumb: TRect;'
          '    HScroll: TRect;'
          '    LeftArrow: TRect;'
          '    RightArrow: TRect;'
          '    HThumb: TRect;'
          '    Intersect: TRect;'
          '  end;'
          ''
          
            '  TScrollHitTest = (shtNoWhere, shtBorder, shtLeftArrow, shtRigh' +
            'tArrow, shtHorzThumb, shtPageLeft, shtPageRight,'
          
            '    shtTopArrow, shtBottomArrow, shtVertThumb, shtPageUp, shtPag' +
            'eDown);'
          ''
          '  TFsScrollContainer = class(TCustomControl)'
          '  private'
          '    FTimer: TTimer;'
          '    FCaptureRegion: TScrollHitTest;'
          '    FCapturePoint: TPoint;'
          '    FScrollBarDrawer: TFsCustomScrollBar;'
          '    FRealControl: TControl;'
          
            '    procedure SetScrollBarDrawer(const Value: TFsCustomScrollBar' +
            ');'
          '    function GetRealScrollBar: TFsCustomScrollBar;'
          '    function NeedScrollBar(out HScroll: Boolean): Boolean;'
          
            '    procedure GetScrollInfo(var fsi: TFsScrollInfo; var vsi, hsi' +
            ': TScrollInfo);'
          '    procedure OnTimer(Sender: TObject);'
          
            '    function ControlMessage(msg: DWORD; wparam, lparam: Integer)' +
            ': Integer;'
          '  protected'
          '    FMouseInControl: Boolean;'
          '    procedure WMSize(var msgr: TWMSize); message WM_SIZE;'
          
            '    procedure CMMouseEnter(var msgr: TMessage); message CM_MOUSE' +
            'ENTER;'
          
            '    procedure CMMouseLeave(var msgr: TMessage); message CM_MOUSE' +
            'LEAVE;'
          
            '    procedure MouseDown(Button: TMouseButton; Shift: TShiftState' +
            '; X, Y: Integer); override;'
          
            '    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; ' +
            'X, Y: Integer); override;'
          
            '    procedure MouseMove(Shift: TShiftState; X, Y: Integer); over' +
            'ride;'
          '    procedure CreateParams(var Params: TCreateParams); override;'
          '    procedure Paint; override;'
          ''
          
            '    function GetControlScrollInfo(var si: TScrollInfo; isVert: B' +
            'oolean): Boolean; virtual;'
          '    function CreateRealControl: TControl; virtual; abstract;'
          '  public'
          '    constructor Create(AOwner: TComponent); override;'
          '    destructor Destroy; override;'
          '    procedure AdjustInnerControlBounds;'
          '    function HitTest(X, Y: Integer): TScrollHitTest; '
          '    property Canvas;'
          '    property RealControl: TControl read FRealControl;'
          '  published'
          '    property Align;'
          '    property Anchors;'
          
            '    property ScrollBarDrawer: TFsCustomScrollBar read FScrollBar' +
            'Drawer write SetScrollBarDrawer;'
          '  end;'
          ''
          '  TFsMemo = class(TFsScrollContainer)'
          '  private'
          '    function GetScrollBars: TScrollStyle;'
          '    procedure SetScrollBars(const Value: TScrollStyle);'
          '    function GetLines: TStrings;'
          '    procedure SetLines(const Value: TStrings);'
          '  protected'
          '    function CreateRealControl: TControl; override;'
          '  published'
          
            '    property ScrollBars: TScrollStyle read GetScrollBars write S' +
            'etScrollBars;'
          '    property Lines: TStrings read GetLines write SetLines;'
          '  end;'
          ''
          '  TFsListBox = class(TFsScrollContainer)'
          '  private'
          '    function GetItems: TStrings;'
          '    procedure SetItems(const Value: TStrings);'
          '  protected'
          '    function CreateRealControl: TControl; override;'
          '  published'
          '    property Items: TStrings read GetItems write SetItems;'
          '  end;'
          '  '
          '  TFsEdit = class(TCustomEdit)'
          '  private'
          '    FMouseInBorderColor: TColor;'
          '    procedure SetMouseInBorderColor(const Value: TColor);'
          '  protected'
          '    FMouseIn: Boolean;'
          
            '    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEE' +
            'NTER;'
          
            '    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSEL' +
            'EAVE;'
          
            '    procedure WMNCPAINT(var msg: TWMNCPaint); message WM_NCPAINT' +
            ';'
          '  public'
          '    constructor Create(AOwner: TComponent); override;'
          '  published'
          
            '    property MouseInBorderColor: TColor read FMouseInBorderColor' +
            ' write SetMouseInBorderColor;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSelect;'
          '    property AutoSize;'
          '    property BevelEdges;'
          '    property BevelInner;'
          '    property BevelKind default bkNone;'
          '    property BevelOuter;'
          '    property BevelWidth;'
          '    property BiDiMode;'
          '    property BorderStyle;'
          '    property CharCase;'
          '    property Color;'
          '    property Constraints;'
          '    property Ctl3D;'
          '    property DoubleBuffered;'
          '    property DragCursor;'
          '    property DragKind;'
          '    property DragMode;'
          '    property Enabled;'
          '    property Font;'
          '    property HideSelection;'
          '    property ImeMode;'
          '    property ImeName;'
          '    property MaxLength;'
          '    property OEMConvert;'
          '    property ParentBiDiMode;'
          '    property ParentColor;'
          '    property ParentCtl3D;'
          '    property ParentFont;'
          '    property ParentShowHint;'
          '    property PasswordChar;'
          '    property PopupMenu;'
          '    property ReadOnly;'
          '    property ShowHint;'
          '    property TabOrder;'
          '    property TabStop;'
          '    property Text;'
          '    property Visible;'
          '    property OnChange;'
          '    property OnClick;'
          '    property OnContextPopup;'
          '    property OnDblClick;'
          '    property OnDragDrop;'
          '    property OnDragOver;'
          '    property OnEndDock;'
          '    property OnEndDrag;'
          '    property OnEnter;'
          '    property OnExit;'
          '    property OnKeyDown;'
          '    property OnKeyPress;'
          '    property OnKeyUp;'
          '    property OnMouseActivate;'
          '    property OnMouseDown;'
          '    property OnMouseEnter;'
          '    property OnMouseLeave;'
          '    property OnMouseMove;'
          '    property OnMouseUp;'
          '    property OnStartDock;'
          '    property OnStartDrag;'
          '  end;'
          ''
          '  TFsButtonEdit = class(TFsEdit)'
          '  private'
          '    FOnClickButton: TNotifyEvent;'
          '    FButtonPicture: TPicture;'
          '    procedure WriteButtonPicture(const Value: TPicture);'
          '  protected'
          '    FNCCanvas: TCanvas;'
          
            '    procedure WMNCPAINT(var msgr: TWMNCPaint); message WM_NCPAIN' +
            'T;'
          
            '    procedure WMNCLButtonDown(var msgr: TWMNCLButtonDown); messa' +
            'ge WM_NCLBUTTONDOWN;'
          
            '    procedure WMNCHitTest(var msgr: TWMNCHitTest); message WM_NC' +
            'HITTEST;'
          
            '    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_' +
            'NCCALCSIZE;'
          '  public'
          '    constructor Create(AOwner: TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          
            '    property ButtonPicture: TPicture read FButtonPicture write W' +
            'riteButtonPicture;'
          
            '    property OnClickButton: TNotifyEvent read FOnClickButton wri' +
            'te FOnClickButton;'
          '  end;'
          ''
          '  TFsNCScrollMemo = class(TCustomMemo)'
          '  private'
          '    FMouseInBorderColor: TColor;'
          '    FMouseIn: Boolean;'
          '    FScrollBar: TFsCustomScrollBar;'
          '    procedure PaintNC;'
          
            '    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEE' +
            'NTER;'
          
            '    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSEL' +
            'EAVE;'
          
            '    procedure WMNCPAINT(var msgr: TWMNCPaint); message WM_NCPAIN' +
            'T;'
          
            '    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_' +
            'NCCALCSIZE;'
          
            '    procedure WMNCHitTest(var msgr: TWMNCHitTest); message WM_NC' +
            'HITTEST;'
          
            '    procedure WMNCLButtonDown(var msgr: TWMNCLButtonDown); messa' +
            'ge WM_NCLBUTTONDOWN;'
          '    procedure SetMouseInBorderColor(const Value: TColor);'
          '    procedure SetScrollBar(const Value: TFsCustomScrollBar);'
          '    function GetRealScrollBar: TFsCustomScrollBar;'
          '  protected'
          
            '    procedure GetScrollRect(var rcVScroll, rcHScroll, rcIntersec' +
            't: TRect);'
          '    function GetVScrollRect(var rc: TRect): Boolean;'
          '    function GetHScrollRect(var rc: TRect): Boolean;'
          '    procedure WndProc(var msgr: TMessage); override;'
          
            '    procedure Notification(AComponent: TComponent; Operation: TO' +
            'peration); override;'
          '  public'
          '    constructor Create(AOwner: TComponent); override;'
          '  published'
          
            '    property ScrollBar: TFsCustomScrollBar read FScrollBar write' +
            ' SetScrollBar;'
          
            '    property MouseInBorderColor: TColor read FMouseInBorderColor' +
            ' write SetMouseInBorderColor;'
          '    property Align;'
          '    property Alignment;'
          '    property Anchors;'
          '    property BevelEdges;'
          '    property BevelInner;'
          '    property BevelKind default bkNone;'
          '    property BevelOuter;'
          '    property BiDiMode;'
          '    property BorderStyle;'
          '    property Color;'
          '    property Constraints;'
          '    property Ctl3D;'
          '    property DragCursor;'
          '    property DragKind;'
          '    property DragMode;'
          '    property Enabled;'
          '    property Font;'
          '    property HideSelection;'
          '    property ImeMode;'
          '    property ImeName;'
          '    property Lines;'
          '    property MaxLength;'
          '    property OEMConvert;'
          '    property ParentBiDiMode;'
          '    property ParentColor;'
          '    property ParentCtl3D;'
          '    property ParentFont;'
          '    property ParentShowHint;'
          '    property PopupMenu;'
          '    property ReadOnly;'
          '    property ScrollBars;'
          '    property ShowHint;'
          '    property TabOrder;'
          '    property TabStop;'
          '    property Visible;'
          '    property WantReturns;'
          '    property WantTabs;'
          '    property WordWrap;'
          '    property OnChange;'
          '    property OnClick;'
          '    property OnContextPopup;'
          '    property OnDblClick;'
          '    property OnDragDrop;'
          '    property OnDragOver;'
          '    property OnEndDock;'
          '    property OnEndDrag;'
          '    property OnEnter;'
          '    property OnExit;'
          '    property OnKeyDown;'
          '    property OnKeyPress;'
          '    property OnKeyUp;'
          '    property OnMouseActivate;'
          '    property OnMouseDown;'
          '    property OnMouseEnter;'
          '    property OnMouseLeave;'
          '    property OnMouseMove;'
          '    property OnMouseUp;'
          '    property OnStartDock;'
          '    property OnStartDrag;'
          '  end;'
          ''
          '  TFsBorderlessMemo = class(TCustomMemo)'
          '  protected'
          '    procedure WndProc(var msgr: TMessage); override;'
          
            '    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_' +
            'NCCALCSIZE;'
          
            '    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAIN' +
            'T;'
          '  end;'
          ''
          '  TFsBorderlessListBox = class(TCustomListBox)'
          '  protected'
          '    procedure WndProc(var msgr: TMessage); override;'
          
            '    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_' +
            'NCCALCSIZE;'
          
            '    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAIN' +
            'T;'
          '  end;'
          ''
          '  TFsCustomCheckBox = class(TFsGraphicControl)'
          '  private'
          '    FChecked: Boolean;'
          '    FSpace: Integer;'
          '    procedure SetChecked(const Value: Boolean);'
          '    procedure SetSpace(const Value: Integer);'
          '  protected'
          
            '    procedure CMTextChange(var msg: TMessage); message CM_TEXTCH' +
            'ANGED;'
          
            '    procedure CMFontChange(var msg: TMessage); message CM_FONTCH' +
            'ANGED;'
          '    procedure GetImageSize(out w, h: Integer); virtual;'
          '    procedure DrawMark(const Rect: TRect); virtual; abstract;'
          '    procedure Paint; override;'
          
            '    procedure MouseDown(Button: TMouseButton; Shift: TShiftState' +
            '; X, Y: Integer); override;'
          '    procedure GetContentDimension(out dim: TSize); override;'
          '  public'
          '    constructor Create(Owner: TComponent); override;'
          '  published'
          '    property Action;'
          '    property Align;'
          '    property Anchors;'
          '    property AutoSize;'
          '    property Enabled;'
          '    property Font;'
          '    property Hint;'
          '    property ShowHint;'
          '    property Visible;'
          '    property OnClick;'
          '    property Caption;'
          '    property Checked: Boolean read FChecked write SetChecked;'
          '    property Space: Integer read FSpace write SetSpace;'
          '  end;'
          ''
          '  TFsCheckBox = class(TFsCustomCheckBox)'
          '  private'
          '    FCheckedPicture: TFsPictureDrawable;'
          '    FUnCheckedPicture: TFsPictureDrawable;'
          '    procedure SetCheckedPicture(const Value: TPicture);'
          '    procedure SetUnCheckedPicture(const Value: TPicture);'
          '    function GetCheckedPicture: TPicture;'
          '    function GetUnCheckedPicture: TPicture;'
          '    procedure PictureChanged(Sender: TObject; ID: TNotifyID);'
          '  protected'
          '    procedure GetImageSize(out w, h: Integer); override;'
          '    procedure DrawMark(const Rect: TRect); override;'
          '  public'
          '    constructor Create(Owner: TComponent); override;'
          '    destructor Destroy; override;'
          '  published'
          
            '    property CheckedPicture: TPicture read GetCheckedPicture wri' +
            'te SetCheckedPicture;'
          
            '    property UnCheckedPicture: TPicture read GetUnCheckedPicture' +
            ' write SetUnCheckedPicture;'
          '  end;'
          ''
          '  TFsSkinCheckBox = class(TFsCustomCheckBox)'
          '  '
          '  end;'
          ''
          'function GetDefaultScrollBar: TFsCustomScrollBar;'
          ''
          'implementation'
          ''
          'type'
          '  TControlHack = class(TControl)'
          '  '
          '  end;'
          ''
          'var'
          '  DefaultScrollBar: TFsCustomScrollBar;'
          ''
          'function GetDefaultScrollBar: TFsCustomScrollBar;'
          'begin'
          '  if not Assigned(DefaultScrollBar) then'
          '    DefaultScrollBar := TFsFlatScrollBar.Create(nil);'
          ''
          '  Result := DefaultScrollBar;'
          'end;'
          ''
          '{ TFsCustomButton }'
          ''
          'procedure TFsCustomButton.CMFontChanged(var msgr: TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCustomButton.CMMouseEnter(var msg: TMessage);'
          'begin'
          '  inherited;'
          ''
          '  Include(FMouseFlag, mfMouseOver);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCustomButton.CMMouseLeave(var msg: TMessage);'
          'begin'
          '  inherited;'
          ''
          '  Exclude(FMouseFlag, mfMouseOver);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCustomButton.CMTextChange(var msg: TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'constructor TFsCustomButton.Create(Owner: TComponent);'
          'begin'
          '  inherited;'
          '  ControlStyle := ControlStyle + [csReplicatable, csPannable];'
          '  FSpace := 4;'
          '  FShowCaption := True;'
          '  FLayout := blTextInStretchPicture;'
          'end;'
          ''
          'procedure TFsCustomButton.DrawButtonSurface;'
          'var'
          '  flags: DWORD;'
          'begin'
          '  flags := DFCS_BUTTONPUSH;'
          ''
          '  if mfLButtonDown in Self.FMouseFlag then'
          '    flags := flags or DFCS_PUSHED;'
          ''
          
            '  Windows.DrawFrameControl(Self.Canvas.Handle, Rect(0, 0, Self.W' +
            'idth, Self.Height),'
          '    DFC_BUTTON, flags);'
          'end;'
          ''
          'procedure TFsCustomButton.DrawImageAndText;'
          'var'
          '  w: Integer;'
          'begin'
          '  Self.GetPictureSize(w);'
          '  '
          '  if w = 0 then Self.DrawButtonSurface;'
          ''
          '  case Self.FLayout of'
          '    blPictureLeft: Self.DrawPictureLeft;'
          '    blPictureRight: Self.DrawPictureRight;'
          '    blPictureTop: Self.DrawPictureTop;'
          '    blPictureBottom: Self.DrawPictureBottom;'
          '    blTextInPicture: Self.DrawTextInPicture;'
          '    blTextInStretchPicture: Self.DrawTextInStretchPicture;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.DrawPictureBottom;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  dim: TSize;'
          '  w, h: Integer;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    rpic.Bottom := Self.Height - (Self.Height - dim.cy) div 2;'
          '    rpic.Top := rpic.Bottom - h;'
          ''
          '    if PictureIsHorzStretchable and (dim.cx > w) then'
          '    begin'
          '      rpic.Left := (Self.Width - dim.cx) div 2;'
          '      rpic.Right := rpic.Left + dim.cx;'
          '    end'
          '    else begin'
          '      rpic.Left := (Self.Width - w) div 2;'
          '      rpic.Right := rpic.Left + w;'
          '    end;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Top := rpic.top - FSpace;'
          '  end'
          '  else rpic.Top := Self.Height - (Self.Height - dim.cy) div 2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Top := (Self.Height - dim.cy) div 2;'
          '    rtext.Bottom := rpic.Top;'
          ''
          '    rtext.Left := (Self.Width - dim.cx) div 2;'
          '    rtext.Right := Self.Width - rtext.Left;'
          '    '
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          
            '    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tf' +
            'SingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.DrawPictureLeft;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  dim: TSize;'
          '  w, h: Integer;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    rpic.Left := (Self.Width - dim.cx) div 2;'
          ''
          '    if PictureIsVertStretchable and (dim.cy > h) then'
          '    begin'
          '      rpic.Top := (Self.Height - dim.cy) div 2;'
          '      rpic.Bottom := rpic.Top + dim.cy;'
          '    end'
          '    else begin'
          '      rpic.Top := (Self.Height - h) div 2;'
          '      rpic.Bottom := rpic.Top + h;'
          '    end;'
          ''
          '    rpic.Right := rpic.Left + w;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Right := rpic.Right + FSpace;'
          '  end'
          '  else rpic.Right := (Self.Width - dim.cx) div 2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Left := rpic.Right;'
          '    rtext.Right := Self.Width - (Self.Width - dim.cx) div 2;'
          '    rtext.Top := (Self.Height - dim.cy) div 2;'
          '    rtext.Bottom := Self.Height - (Self.Height - dim.cy) div 2;'
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          
            '    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tf' +
            'SingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.DrawPictureRight;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  w, h: Integer;'
          '  dim: TSize;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          '  '
          '  if w > 0 then'
          '  begin'
          '    rpic.Right := Self.Width - (Self.Width - dim.cx) div 2;'
          ''
          '    if PictureIsVertStretchable and (dim.cy > h) then'
          '    begin'
          '      rpic.Top := (Self.Height - dim.cy) div 2;'
          '      rpic.Bottom := rpic.Top + dim.cy;'
          '    end'
          '    else begin'
          '      rpic.Top := (Self.Height - h) div 2;'
          '      rpic.Bottom := rpic.Top + h;'
          '    end;'
          ''
          '    rpic.Left := rpic.Right - w;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Left := rpic.Left - FSpace;'
          '  end'
          '  else rpic.Left := Self.Width - (Self.Width - dim.cx) div 2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Left := (Self.Width - dim.cx) div 2;'
          '    rtext.Right := rpic.Left;'
          '    rtext.Top := (Self.Height - dim.cy) div 2;'
          '    rtext.Bottom := Self.Height - (Self.Height - dim.cy) div 2;'
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          
            '    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tf' +
            'SingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.DrawPictureTop;'
          'var'
          '  Text: string;'
          '  rtext, rpic: TRect;'
          '  w, h: Integer;'
          '  dim: TSize;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    rpic.Top := (Self.Height - dim.cy) div 2;'
          '    rpic.Bottom := rpic.Top + h;'
          ''
          '    if PictureIsHorzStretchable and (dim.cx > w) then'
          '    begin'
          '      rpic.Left := (Self.Width - dim.cx) div 2;'
          '      rpic.Right := rpic.Left + dim.cx;'
          '    end'
          '    else begin'
          '      rpic.Left := (Self.Width - w) div 2;'
          '      rpic.Right := rpic.Left + w;'
          '    end;'
          ''
          '    Self.DrawPicture(rpic);'
          ''
          '    rpic.Bottom := rpic.Bottom + FSpace;'
          '  end'
          '  else rpic.Bottom := (Self.Height - dim.cy) div 2;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') then'
          '  begin'
          '    Text := Self.Caption;'
          ''
          '    rtext.Top := rpic.Bottom;'
          '    rtext.Bottom := Self.Height - (Self.Height - dim.cy) div 2;'
          ''
          '    rtext.Left := (Self.Width - dim.cx) div 2;'
          '    rtext.Right := Self.Width - rtext.Left;'
          '    '
          '    Canvas.Font := Self.Font;'
          '    Canvas.Brush.Style := bsClear;'
          
            '    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tf' +
            'SingleLine]);'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.DrawTextInPicture;'
          'var'
          '  Text: string;'
          '  r: TRect;'
          '  w, h: Integer;'
          '  dim: TSize;'
          'begin'
          '  Self.GetContentDimension(dim);'
          ''
          '  h := Self.GetPictureSize(w);'
          ''
          '  if w > 0 then'
          '  begin'
          '    if PictureIsHorzStretchable and (w < dim.cx) then'
          '      r.Left := (Self.Width - dim.cx) div 2'
          '    else'
          '      r.Left := (Self.Width - w) div 2;'
          ''
          '    r.Right := Self.Width - r.Left;'
          ''
          '    if PictureIsVertStretchable and (h < dim.cy) then'
          '      r.Top := (Self.Height - dim.cy) div 2'
          '    else'
          '      r.Top := (Self.Height - h) div 2;'
          ''
          '    r.Bottom := Self.Height - r.Top;'
          '    '
          '    Self.DrawPicture(r);'
          '  end;'
          ''
          '  if FShowCaption then'
          '  begin'
          '    Text := Caption;'
          ''
          '    if Text <> '#39#39' then'
          '    begin'
          '      r.Left := 0;'
          '      r.Right := Self.Width;'
          '      r.Top := 0;'
          '      r.Bottom := Self.Height;'
          ''
          '      Canvas.Font := Self.Font;'
          '      Canvas.Brush.Style := bsClear;'
          
            '      Canvas.TextRect(r, Text, [tfCenter, tfVerticalCenter, tfSi' +
            'ngleLine]);'
          '    end;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.DrawTextInStretchPicture;'
          'var'
          '  Text: string;'
          '  r: TRect;'
          '  w: Integer;'
          'begin'
          '  r.Left := 0;'
          '  r.Right := Self.Width;'
          '  r.Top := 0;'
          '  r.Bottom := Self.Height;'
          ''
          '  Self.GetPictureSize(w);'
          ''
          '  if w > 0 then Self.DrawPicture(r);'
          ''
          '  if FShowCaption then'
          '  begin'
          '    Text := Caption;'
          ''
          '    if Text <> '#39#39' then'
          '    begin'
          '      Canvas.Font := Self.Font;'
          '      Canvas.Brush.Style := bsClear;'
          
            '      Canvas.TextRect(r, Text, [tfCenter, tfVerticalCenter, tfSi' +
            'ngleLine]);'
          '    end;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.GetContentDimension(out dim: TSize);'
          'var'
          '  TextSize: TSize;'
          '  ImageSize: TSize;'
          'begin'
          '  dim.cx := 0;'
          '  dim.cy := 0;'
          ''
          '  if FShowCaption and (Self.Caption <> '#39#39') then'
          '  begin'
          '    Self.Canvas.Font := Self.Font;'
          '    TextSize := Self.Canvas.TextExtent(Self.Caption);'
          '  end'
          '  else begin'
          '    TextSize.cx := 0;'
          '    TextSize.cy := 0;'
          '  end;'
          ''
          '  ImageSize.cy := Self.GetPictureSize(ImageSize.cx);'
          ''
          '  if (ImageSize.cx = 0) and (TextSize.cx = 0) then Exit;'
          ''
          '  case FLayout of'
          '    blPictureLeft, blPictureRight:'
          '      begin'
          '        if ImageSize.cx > 0 then'
          '        begin'
          
            '          if TextSize.cx > 0 then dim.cx := ImageSize.cx + FSpac' +
            'e + TextSize.cx'
          '          else dim.cx := ImageSize.cx;'
          '        end'
          '        else begin'
          '          if TextSize.cx > 0 then dim.cx := TextSize.cx'
          '          else dim.cx := 0;'
          '        end;'
          ''
          '        if TextSize.cy > ImageSize.cy then dim.cy := TextSize.cy'
          '        else dim.cy := ImageSize.cy;'
          '      end;'
          ''
          '    blPictureTop, blPictureBottom:'
          '      begin'
          '        if ImageSize.cy > 0 then'
          '        begin'
          
            '          if TextSize.cy > 0 then dim.cy := ImageSize.cy + FSpac' +
            'e + TextSize.cy'
          '          else dim.cy := ImageSize.cy;'
          '        end'
          '        else begin'
          '          if TextSize.cy > 0 then dim.cy := TextSize.cy'
          '          else dim.cy := 0;'
          '        end;'
          ''
          '        if TextSize.cx > ImageSize.cx then dim.cx := TextSize.cx'
          '        else dim.cx := ImageSize.cx;'
          '      end;'
          ''
          '    blTextInPicture, blTextInStretchPicture:'
          '      begin'
          '        if TextSize.cx > ImageSize.cx then dim.cx := TextSize.cx'
          '        else dim.cx := ImageSize.cx;'
          ''
          '        if TextSize.cy > ImageSize.cy then dim.cy := TextSize.cy'
          '        else dim.cy := ImageSize.cy;'
          '      end;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsCustomButton.MouseDown(Button: TMouseButton; Shift:' +
            ' TShiftState; X, Y: Integer);'
          'begin'
          '  inherited;'
          ''
          '  if Button = mbLeft then Include(FMouseFlag, mfLButtonDown);'
          ''
          '  if Button = mbRight then Include(FMouseFlag, mfRButtonDown);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          
            'procedure TFsCustomButton.MouseUp(Button: TMouseButton; Shift: T' +
            'ShiftState; X, Y: Integer);'
          'begin'
          '  inherited;'
          '  '
          '  if Button = mbLeft then Exclude(FMouseFlag, mfLButtonDown);'
          ''
          '  if Button = mbRight then Exclude(FMouseFlag, mfRButtonDown);'
          ''
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCustomButton.Paint;'
          'begin'
          '  Self.DrawImageAndText;'
          'end;'
          ''
          'function TFsCustomButton.PictureIsHorzStretchable: Boolean;'
          'begin'
          '  Result := False;'
          'end;'
          ''
          'function TFsCustomButton.PictureIsVertStretchable: Boolean;'
          'begin'
          '  Result := False;'
          'end;'
          ''
          
            'procedure TFsCustomButton.SetLayout(const Value: TFsImageButtonL' +
            'ayout);'
          'begin'
          '  if FLayout <> Value then'
          '  begin'
          '    FLayout := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.SetSpace(const Value: Integer);'
          'begin'
          '  if (FSpace <> Value) and (Value >= 0) then'
          '  begin'
          '    FSpace := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomButton.SetShowCaption(const Value: Boolean);'
          'begin'
          '  if FShowCaption <> Value then'
          '  begin'
          '    FShowCaption := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          '{ TFsImageButton }'
          ''
          'constructor TFsImageButton.Create(Owner: TComponent);'
          'begin'
          '  inherited;'
          '  FPicture := TFsPictureDrawable.Create(Self);'
          '  FDisablePicture := TFsPictureDrawable.Create(Self);'
          '  FMouseOverPicture := TFsPictureDrawable.Create(Self);'
          '  FMouseDownPicture := TFsPictureDrawable.Create(Self);'
          '  FPicture.AddOnChangeListener(Self.PictureChanged);'
          '  FDisablePicture.AddOnChangeListener(Self.PictureChanged);'
          '  FMouseOverPicture.AddOnChangeListener(Self.PictureChanged);'
          '  FMouseDownPicture.AddOnChangeListener(Self.PictureChanged);'
          'end;'
          ''
          'destructor TFsImageButton.Destroy;'
          'begin'
          '  FPicture.RemoveOnChangeListener(Self.PictureChanged);'
          '  FDisablePicture.RemoveOnChangeListener(Self.PictureChanged);'
          '  FMouseOverPicture.RemoveOnChangeListener(Self.PictureChanged);'
          '  FMouseDownPicture.RemoveOnChangeListener(Self.PictureChanged);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsImageButton.DrawPicture(const Rect: TRect);'
          'var'
          '  drawable: TFsSingleDrawable;'
          'begin'
          '  drawable := Self.GetDrawable;'
          ''
          '  if Assigned(drawable) then'
          '    drawable.Draw(Self.Canvas, Rect);'
          'end;'
          ''
          'function TFsImageButton.GetDisablePicture: TPicture;'
          'begin'
          '  Result := FDisablePicture.Picture;'
          'end;'
          ''
          'function TFsImageButton.GetDrawable: TFsSingleDrawable;'
          'begin'
          '  if not Self.Enabled then Result := FDisablePicture'
          
            '  else if mfLButtonDown in FMouseFlag then Result := FMouseDownP' +
            'icture'
          
            '  else if mfMouseOver in FMouseFlag then Result := FMouseOverPic' +
            'ture'
          '  else Result := FPicture;'
          ''
          '  if Result.Empty then'
          '    Result := FPicture;'
          'end;'
          ''
          'function TFsImageButton.GetMouseDownPicture: TPicture;'
          'begin'
          '  Result := FMouseDownPicture.Picture;'
          'end;'
          ''
          'function TFsImageButton.GetMouseOverPicture: TPicture;'
          'begin'
          '  Result := FMouseOverPicture.Picture;'
          'end;'
          ''
          'function TFsImageButton.GetPicture: TPicture;'
          'begin'
          '  Result := FPicture.Picture;'
          'end;'
          ''
          
            'function TFsImageButton.GetPictureSize(out width: Integer): Inte' +
            'ger;'
          'var'
          '  drawable: TFsDrawable;'
          'begin'
          '  drawable := Self.GetDrawable;'
          ''
          '  if Assigned(drawable) and not drawable.Empty then'
          '  begin'
          '    width := drawable.Width;'
          '    Result := drawable.Height;'
          '  end'
          '  else begin'
          '    width := 0;'
          '    Result := 0;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsImageButton.PictureChanged(Sender: TObject; ID: TNo' +
            'tifyID);'
          'begin'
          
            '  if (ID = niChange) and (Sender = GetDrawable) then Self.AutoSi' +
            'zeAndInvalidate;'
          'end;'
          ''
          'procedure TFsImageButton.SetPicture(const Value: TPicture);'
          'begin'
          '  FPicture.Picture.Assign(Value);'
          'end;'
          ''
          
            'procedure TFsImageButton.SetDisablePicture(const Value: TPicture' +
            ');'
          'begin'
          '  FDisablePicture.Picture.Assign(Value);'
          'end;'
          ''
          
            'procedure TFsImageButton.SetMouseDownPicture(const Value: TPictu' +
            're);'
          'begin'
          '  FMouseDownPicture.Picture.Assign(Value);'
          'end;'
          ''
          
            'procedure TFsImageButton.SetMouseOverPicture(const Value: TPictu' +
            're);'
          'begin'
          '  FMouseOverPicture.Picture.Assign(Value);'
          'end;'
          ''
          '{ TFsImage }'
          ''
          'constructor TFsImage.Create(Owner: TComponent);'
          'begin'
          '  inherited;'
          '  ControlStyle := ControlStyle + [csReplicatable, csPannable];'
          '  Self.SetBounds(Left, Top, 100, 100);'
          'end;'
          ''
          'destructor TFsImage.Destroy;'
          'begin'
          '  SetPicture(nil);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsImage.GetContentDimension(out dim: TSize);'
          'begin'
          '  if Assigned(FPicture) then'
          '  begin'
          '    dim.cx := FPicture.Width;'
          '    dim.cy := FPicture.Height;'
          '  end'
          '  else begin'
          '    dim.cx := 0;'
          '    dim.cy := 0;'
          '  end;'
          'end;'
          ''
          'procedure TFsImage.Paint;'
          'var'
          '  r: TRect;'
          'begin'
          '  inherited;'
          ''
          '  r := Rect(0, 0, Width, Height);'
          ''
          '  if Assigned(FPicture) and not FPicture.Empty then'
          '  begin'
          '    if FPicture is TFsSingleDrawable then'
          '      TFsSingleDrawable(FPicture).Draw(Canvas, r)'
          '    else'
          
            '      TFsMultiFrameDrawable(FPicture).DrawFrame(Canvas.Handle, r' +
            ', 0);'
          '  end;'
          ''
          '  if csDesigning in ComponentState then'
          '  begin'
          '    Canvas.Pen.Style := psDash;'
          '    Canvas.Brush.Style := bsClear;'
          '    Canvas.Rectangle(0, 0, Width, Height);'
          '  end;'
          'end;'
          ''
          
            'procedure TFsImage.PictureChanged(Sender: TObject; ID: TNotifyID' +
            ');'
          'begin'
          '  if ID = niDestroy then'
          '  begin'
          '    if Sender = FPicture then'
          '    begin'
          '      FPicture := nil;'
          '      Self.AutoSizeAndInvalidate;'
          '    end;'
          '  end'
          '  else if ID = niChange then'
          '  begin'
          '    if Sender = FPicture then Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsImage.SetPicture(const Value: TFsDrawable);'
          'begin'
          '  if FPicture <> Value then'
          '  begin'
          '    if Assigned(FPicture) then'
          '      FPicture.RemoveOnChangeListener(Self.PictureChanged);'
          ''
          '    FPicture := Value;'
          ''
          '    if Assigned(FPicture) then'
          '      FPicture.AddOnChangeListener(Self.PictureChanged);'
          '      '
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          '{ TFsEdit }'
          ''
          'procedure TFsEdit.CMMouseEnter(var msg: TMessage);'
          'begin'
          '  FMouseIn := True;'
          '  inherited;'
          'end;'
          ''
          'procedure TFsEdit.CMMouseLeave(var msg: TMessage);'
          'begin'
          '  FMouseIn := False;'
          '  inherited;'
          'end;'
          ''
          'constructor TFsEdit.Create(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FMouseInBorderColor := RGB(123, 228, 255);'
          'end;'
          ''
          'procedure TFsEdit.SetMouseInBorderColor(const Value: TColor);'
          'begin'
          '  if FMouseInBorderColor <> Value then'
          '  begin'
          '    FMouseInBorderColor := Value;'
          '    Self.Invalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsEdit.WMNCPAINT(var msg: TWMNCPaint);'
          'var'
          '  dc: HDC;'
          '  hb: HBRUSH;'
          '  bgc: DWORD;'
          'begin'
          '  dc := GetWindowDC(Handle);'
          ''
          '  try    '
          '    if FMouseIn then bgc := ColorToRGB(FMouseInBorderColor)'
          '    else bgc := RGB(78, 160, 209);'
          '    '
          '    hb := CreateSolidBrush(bgc);'
          
            '    Windows.FrameRect(dc, Rect(0, 0, Self.Width, Self.Height), h' +
            'b);'
          '    DeleteObject(hb);'
          ''
          '    if FMouseIn then bgc := RGB(78, 160, 209)'
          '    else bgc := ColorToRGB(Self.Color);'
          '    '
          '    hb := CreateSolidBrush(bgc);'
          
            '    Windows.FrameRect(dc, Rect(1, 1, Self.Width - 1, Self.Height' +
            ' - 1), hb);'
          '    DeleteObject(hb);'
          '  finally'
          '    ReleaseDC(Handle, dc);'
          '  end;'
          'end;'
          ''
          '{ TFsCustomCheckBox }'
          ''
          'procedure TFsCustomCheckBox.CMFontChange(var msg: TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCustomCheckBox.CMTextChange(var msg: TMessage);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'constructor TFsCustomCheckBox.Create(Owner: TComponent);'
          'begin'
          '  inherited;'
          '  FSpace := 4;'
          '  ControlStyle := ControlStyle + [csReplicatable, csPannable];'
          'end;'
          ''
          'procedure TFsCustomCheckBox.GetContentDimension(out dim: TSize);'
          'var'
          '  TextSize: TSize;'
          '  ImageWidth, ImageHeight: Integer;'
          'begin'
          '  Canvas.Font := Self.Font;'
          ''
          '  TextSize := Canvas.TextExtent(Self.Caption);'
          ''
          '  Self.GetImageSize(ImageWidth, ImageHeight);'
          ''
          '  if ImageWidth = 0 then'
          '  begin'
          '    ImageWidth := Windows.GetSystemMetrics(SM_CXMENUCHECK);'
          '    ImageHeight := Windows.GetSystemMetrics(SM_CYMENUCHECK);'
          '  end;'
          ''
          '  dim.cx := ImageWidth + FSpace + TextSize.cx;'
          ''
          '  if TextSize.cy > ImageHeight then dim.cy := TextSize.cy'
          '  else dim.cy := ImageHeight;'
          'end;'
          ''
          'procedure TFsCustomCheckBox.GetImageSize(out w, h: Integer);'
          'begin'
          '  w := 0;'
          '  h := 0;'
          'end;'
          ''
          
            'procedure TFsCustomCheckBox.MouseDown(Button: TMouseButton; Shif' +
            't: TShiftState; X, Y: Integer);'
          'begin'
          '  if Button = mbLeft then'
          '  begin'
          '    FChecked := not FChecked;'
          '    Invalidate;'
          '  end;'
          ''
          '  inherited;'
          'end;'
          ''
          'procedure TFsCustomCheckBox.Paint;'
          'var'
          '  w, h: Integer;'
          '  r: TRect;'
          '  _caption: string;'
          'begin'
          '  inherited;'
          ''
          '  Self.GetImageSize(w, h);'
          ''
          '  if w = 0 then'
          '  begin'
          '    w := Windows.GetSystemMetrics(SM_CXMENUCHECK);'
          '    h := Windows.GetSystemMetrics(SM_CYMENUCHECK);'
          ''
          '    r.Left := 0;'
          '    r.Top := (Self.Height - h) div 2;'
          '    r.Right := w;'
          '    r.Bottom := r.Top + h;'
          ''
          '    if Self.FChecked then'
          
            '      Windows.DrawFrameControl(Canvas.Handle, r, DFC_BUTTON, DFC' +
            'S_BUTTONCHECK or DFCS_CHECKED)'
          '    else'
          
            '      Windows.DrawFrameControl(Canvas.Handle, r, DFC_BUTTON, DFC' +
            'S_BUTTONCHECK);'
          '  end'
          '  else begin'
          '    r.Left := 0;'
          '    r.Top := (Self.Height - h) div 2;'
          '    r.Right := w;'
          '    r.Bottom := r.Top + h;'
          '    Self.DrawMark(r);'
          '  end;'
          ''
          '  r := Rect(r.Right + FSpace, 0, Self.Width, Self.Height);'
          ''
          '  Self.Canvas.Font := Self.Font;'
          '  Self.Canvas.Brush.Style := bsClear;'
          '  SetBkMode(Self.Canvas.Handle, Windows.TRANSPARENT);'
          '  _caption := Self.Caption;'
          '  '
          
            '  Self.Canvas.TextRect(r, _caption, [tfVerticalCenter, tfSingleL' +
            'ine]);'
          'end;'
          ''
          'procedure TFsCustomCheckBox.SetChecked(const Value: Boolean);'
          'begin'
          '  if FChecked <> Value then'
          '  begin'
          '    FChecked := Value;'
          '    AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomCheckBox.SetSpace(const Value: Integer);'
          'begin'
          '  if (FSpace <> Value) and (Value >= 0) then'
          '  begin'
          '    FSpace := Value;'
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          '{ TFsCheckBox }'
          ''
          'constructor TFsCheckBox.Create(Owner: TComponent);'
          'begin'
          '  inherited;'
          '  FCheckedPicture := TFsPictureDrawable.Create(Self);'
          '  FUnCheckedPicture := TFsPictureDrawable.Create(Self);'
          '  FCheckedPicture.AddOnChangeListener(Self.PictureChanged);'
          '  FUnCheckedPicture.AddOnChangeListener(Self.PictureChanged);'
          'end;'
          ''
          'destructor TFsCheckBox.Destroy;'
          'begin'
          '  FCheckedPicture.RemoveOnChangeListener(Self.PictureChanged);'
          '  FUnCheckedPicture.RemoveOnChangeListener(Self.PictureChanged);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsCheckBox.DrawMark(const Rect: TRect);'
          'begin'
          '  if Self.Checked then'
          '    FCheckedPicture.Draw(Self.Canvas, Rect)'
          '  else'
          '    FUnCheckedPicture.Draw(Self.Canvas, Rect)'
          'end;'
          ''
          'function TFsCheckBox.GetCheckedPicture: TPicture;'
          'begin'
          '  Result := FCheckedPicture.Picture;'
          'end;'
          ''
          'procedure TFsCheckBox.GetImageSize(out w, h: Integer);'
          'begin'
          '  if Self.Checked then'
          '  begin'
          '    w := FCheckedPicture.Width;'
          '    h := FCheckedPicture.Height;'
          '  end'
          '  else begin'
          '    w := FUnCheckedPicture.Width;'
          '    h := FUnCheckedPicture.Height;'
          '  end;'
          'end;'
          ''
          'function TFsCheckBox.GetUnCheckedPicture: TPicture;'
          'begin'
          '  Result := FUnCheckedPicture.Picture;'
          'end;'
          ''
          
            'procedure TFsCheckBox.PictureChanged(Sender: TObject; ID: TNotif' +
            'yID);'
          'begin'
          '  if ID = niChange then'
          '  begin'
          
            '    if ((Sender = FCheckedPicture) and FChecked) or ((Sender = F' +
            'UnCheckedPicture) and not FChecked) then'
          '      Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCheckBox.SetCheckedPicture(const Value: TPicture);'
          'begin'
          '  FCheckedPicture.Picture.Assign(Value);'
          'end;'
          ''
          
            'procedure TFsCheckBox.SetUnCheckedPicture(const Value: TPicture)' +
            ';'
          'begin'
          '  FUnCheckedPicture.Picture.Assign(Value);'
          'end;'
          ''
          '{ TFsButtonEdit }'
          ''
          'constructor TFsButtonEdit.Create(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FButtonPicture := TPicture.Create;'
          '  FNCCanvas := TCanvas.Create;'
          'end;'
          ''
          'destructor TFsButtonEdit.Destroy;'
          'begin'
          '  FButtonPicture.Free;'
          '  FNCCanvas.Free;'
          '  inherited;'
          'end;'
          ''
          'procedure TFsButtonEdit.WMNCCalcSize(var msgr: TWMNCCalcSize);'
          'begin'
          '  with msgr.CalcSize_Params^ do'
          '  begin'
          '    rgrc[0].Left := rgrc[0].Left + 2;'
          '    rgrc[0].Top := rgrc[0].Top + 2;'
          '    rgrc[0].Right := rgrc[0].Right - 2 - FButtonPicture.Width;'
          '    rgrc[0].Bottom := rgrc[0].Bottom - 2;'
          '  end;'
          ''
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsButtonEdit.WMNCHitTest(var msgr: TWMNCHitTest);'
          'var'
          '  pt: TPoint;'
          'begin'
          '  pt := Self.ScreenToClient(Point(msgr.XPos, msgr.YPos));'
          ''
          
            '  if PtInRect(Rect(Self.Width - 18, 2, Self.Width - 2, Self.Heig' +
            'ht - 2), pt) then msgr.Result := HTBORDER'
          '  else inherited;'
          'end;'
          ''
          
            'procedure TFsButtonEdit.WMNCLButtonDown(var msgr: TWMNCLButtonDo' +
            'wn);'
          'var'
          '  pt: TPoint;'
          'begin'
          '  pt := Self.ScreenToClient(Point(msgr.XCursor, msgr.YCursor));'
          ''
          
            '  if PtInRect(Rect(Self.Width - 18, 2, Self.Width - 2, Self.Heig' +
            'ht - 2), pt) then'
          '  begin'
          '    if Assigned(FOnClickButton) then'
          '      FOnClickButton(Self);'
          ''
          '    msgr.Result := 0;'
          '  end'
          '  else inherited;'
          'end;'
          ''
          'procedure TFsButtonEdit.WMNCPAINT(var msgr: TWMNCPaint);'
          'var'
          '  dc: HDC;'
          '  r: TRect;'
          'begin'
          '  inherited;'
          ''
          '  r.Left := Self.Width - 2 - FButtonPicture.Width;'
          '  r.Right := Self.Width - 2;'
          ''
          '  r.Top := (Self.Height - FButtonPicture.Height) div 2;'
          
            '  r.Bottom := Self.Height - (Self.Height - FButtonPicture.Height' +
            ') div 2;'
          ''
          '  dc := GetWindowDC(Self.Handle);'
          '  '
          '  FNCCanvas.Handle := dc;'
          ''
          '  try'
          '    FNCCanvas.Brush.Color := Self.Color;'
          
            '    FNCCanvas.FillRect(Rect(r.Left, 2, r.Right, Self.Height - 2)' +
            ');'
          '    '
          '    if Assigned(FButtonPicture.Graphic) then'
          '      FNCCanvas.StretchDraw(r, FButtonPicture.Graphic);'
          '  finally'
          '    FNCCanvas.Handle := 0;'
          '    ReleaseDC(Self.Handle, dc);'
          '  end;  '
          'end;'
          ''
          
            'procedure TFsButtonEdit.WriteButtonPicture(const Value: TPicture' +
            ');'
          'begin'
          '  FButtonPicture.Assign(Value);'
          'end;'
          ''
          '{ TFsNCScrollMemo }'
          ''
          'procedure TFsNCScrollMemo.CMMouseEnter(var msg: TMessage);'
          'begin'
          '  inherited;'
          '  FMouseIn := True;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.CMMouseLeave(var msg: TMessage);'
          'begin'
          '  inherited;'
          '  FMouseIn := False;'
          'end;'
          ''
          'constructor TFsNCScrollMemo.Create(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FMouseInBorderColor := RGB(123, 228, 255);'
          'end;'
          ''
          'function TFsNCScrollMemo.GetHScrollRect(var rc: TRect): Boolean;'
          'var'
          '  style: Integer;'
          'begin'
          '  style := GetWindowLong(Handle, GWL_STYLE);'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    rc.Bottom := Self.Height - 2;'
          '    rc.Top := rc.Bottom - GetRealScrollBar.HScrollHeight;'
          '    rc.Left := 2;'
          ''
          '    if style and WS_VSCROLL = 0 then rc.Right := Self.Width - 2'
          
            '    else rc.Right := Self.Width - 2 - GetRealScrollBar.VScrollWi' +
            'dth;'
          ''
          '    Result := True;'
          '  end'
          '  else Result := False;'
          'end;'
          ''
          'function TFsNCScrollMemo.GetRealScrollBar: TFsCustomScrollBar;'
          'begin'
          '  if Assigned(FScrollBar) then Result := FScrollBar'
          '  else Result := GetDefaultScrollBar;'
          'end;'
          ''
          
            'procedure TFsNCScrollMemo.GetScrollRect(var rcVScroll, rcHScroll' +
            ', rcIntersect: TRect);'
          'var'
          '  style: Integer;'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  sb := GetRealScrollBar;'
          '  style := GetWindowLong(Handle, GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    rcVScroll.Right := Self.Width - 2;'
          '    rcVScroll.Left := rcVScroll.Right - sb.VScrollWidth;'
          '    rcVScroll.Top := 2;'
          ''
          
            '    if style and WS_HSCROLL = 0 then rcVScroll.Bottom := Self.He' +
            'ight - 2'
          '    else rcVScroll.Bottom := Self.Height - 2 - sb.HScrollHeight;'
          '  end'
          '  else begin'
          '    rcVScroll.Left := 0;'
          '    rcVScroll.Right := -1;'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    rcHScroll.Bottom := Self.Height - 2;'
          '    rcHScroll.Top := rcHScroll.Bottom - sb.HScrollHeight;'
          '    rcHScroll.Left := 2;'
          ''
          
            '    if style and WS_VSCROLL = 0 then rcHScroll.Right := Self.Wid' +
            'th - 2'
          '    else rcHScroll.Right := Self.Width - 2 - sb.VScrollWidth;'
          '  end'
          '  else begin'
          '    rcHScroll.Left := 0;'
          '    rcHScroll.Right := -1;'
          '  end;'
          ''
          
            '  if (style and WS_HSCROLL <> 0) and (style and WS_VSCROLL <> 0)' +
            ' then'
          '  begin'
          '    rcIntersect.Left := Self.Width - 2 - sb.VScrollWidth;'
          '    rcIntersect.Right := Self.Width - 2;'
          '    rcIntersect.Top := Self.Height - 2 - sb.HScrollHeight;'
          '    rcIntersect.Bottom := Self.Height - 2;'
          '  end'
          '  else begin'
          '    rcIntersect.Left := 0;'
          '    rcIntersect.Right := -1;'
          '  end;'
          '  '
          'end;'
          ''
          'function TFsNCScrollMemo.GetVScrollRect(var rc: TRect): Boolean;'
          'var'
          '  style: Integer;'
          'begin'
          '  style := GetWindowLong(Handle, GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    rc.Right := Self.Width - 2;'
          '    rc.Left := rc.Right - GetRealScrollBar.VScrollWidth;'
          '    rc.Top := 2;'
          ''
          
            '    if style and WS_HSCROLL = 0 then rc.Bottom := Self.Height - ' +
            '2'
          
            '    else rc.Bottom := Self.Height - 2 - GetRealScrollBar.HScroll' +
            'Height;'
          ''
          '    Result := True;'
          '  end'
          '  else Result := False;'
          'end;'
          ''
          
            'procedure TFsNCScrollMemo.Notification(AComponent: TComponent; O' +
            'peration: TOperation);'
          'begin'
          '  inherited;'
          ''
          '  if (Operation = opRemove) and (AComponent = FScrollBar) then'
          '    SetScrollBar(nil);'
          'end;'
          ''
          'procedure TFsNCScrollMemo.PaintNC;'
          'var'
          '  dc: HDC;'
          '  hb: HBRUSH;'
          '  r, rcIntersect, rcVScroll, rcHScroll: TRect;'
          '  bgc: DWORD;'
          '  si: TScrollInfo;'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  dc := GetWindowDC(Handle);'
          ''
          '  try'
          '    if FMouseIn then bgc := ColorToRGB(FMouseInBorderColor)'
          '    else bgc := RGB(78, 160, 209);'
          ''
          '    r.Left := 0;'
          '    r.Top := 0;'
          '    r.Right := Self.Width;'
          '    r.Bottom := Self.Height;'
          ''
          '    hb := CreateSolidBrush(bgc);'
          '    Windows.FrameRect(dc, r, hb);'
          '    DeleteObject(hb);'
          ''
          '    if FMouseIn then bgc := RGB(78, 160, 209)  '
          '    else bgc := ColorToRGB(Self.Color);'
          ''
          '    r.Left := 1;'
          '    r.Top := 1;'
          '    r.Right := Self.Width - 1;'
          '    r.Bottom := Self.Height - 1;'
          ''
          '    hb := CreateSolidBrush(bgc);'
          '    FrameRect(dc, r, hb);'
          '    DeleteObject(hb);'
          ''
          '    sb := GetRealScrollBar;'
          '    '
          '    Self.GetScrollRect(rcVScroll, rcHScroll, rcIntersect);'
          ''
          '    if rcVScroll.Left < rcVScroll.Right then'
          '    begin'
          '      si.cbSize := SizeOf(si);'
          '      si.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '      Windows.GetScrollInfo(Handle, SB_VERT, si);'
          '      //sb.DrawVScroll(dc, si, rcVScroll);'
          '    end;'
          ''
          '    if rcHScroll.Left < rcHScroll.Right then'
          '    begin'
          '      si.cbSize := SizeOf(si);'
          '      si.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '      Windows.GetScrollInfo(Handle, SB_HORZ, si);'
          '      //sb.DrawHScroll(dc, si, rcHScroll);'
          '    end;'
          ''
          '    if rcIntersect.Left < rcIntersect.Right then'
          '      sb.DrawIntersect(dc, rcIntersect);'
          ''
          '  finally'
          '    ReleaseDC(Handle, dc);'
          '  end;'
          'end;'
          ''
          
            'procedure TFsNCScrollMemo.SetMouseInBorderColor(const Value: TCo' +
            'lor);'
          'begin'
          '  if FMouseInBorderColor <> Value then'
          '  begin'
          '    FMouseInBorderColor := Value;'
          '    Self.Invalidate;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsNCScrollMemo.SetScrollBar(const Value: TFsCustomScr' +
            'ollBar);'
          'begin'
          '  if Value <> FScrollBar then'
          '  begin'
          '    if Assigned(FScrollBar) then'
          '      FScrollBar.RemoveFreeNotification(Self);'
          ''
          '    FScrollBar := Value;'
          ''
          '    if Assigned(FScrollBar) then'
          '      FScrollBar.FreeNotification(Self);'
          ''
          '    Self.RecreateWnd;'
          '  end;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WMNCCalcSize(var msgr: TWMNCCalcSize);'
          'var'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  with msgr.CalcSize_Params^ do'
          '  begin'
          '    rgrc[0].Left := rgrc[0].Left + 2;'
          '    rgrc[0].Top := rgrc[0].Top + 2;'
          '    rgrc[0].Right := rgrc[0].Right - 2;'
          '    rgrc[0].Bottom := rgrc[0].Bottom - 2;'
          ''
          '    sb := Self.GetRealScrollBar;'
          '    '
          
            '    if (GetWindowLong(Self.Handle, GWL_STYLE) and WS_VSCROLL) <>' +
            ' 0 then'
          '      Dec(rgrc[0].Right, sb.VScrollWidth);'
          ''
          
            '    if (GetWindowLong(Self.Handle, GWL_STYLE) and WS_HSCROLL) <>' +
            ' 0 then'
          '      Dec(rgrc[0].Bottom, sb.HScrollHeight);'
          '  end;'
          ''
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WMNCHitTest(var msgr: TWMNCHitTest);'
          'var'
          '  pt: TPoint;'
          '  rcClient: TRect;'
          'begin'
          '  inherited;'
          ''
          '  pt.X := msgr.XPos;'
          '  pt.Y := msgr.YPos;'
          '  Windows.ScreenToClient(Self.Handle, pt);'
          ''
          '  Windows.GetClientRect(Self.Handle, rcClient);'
          ''
          '  if PtInRect(rcClient, pt) then msgr.Result := HTCLIENT'
          '  else begin'
          
            '    //PtInRect(Rect(rcClient.Right, 0, rcClient.Right + GetRealS' +
            'crollBar.))'
          '  end;'
          'end;'
          ''
          
            'procedure TFsNCScrollMemo.WMNCLButtonDown(var msgr: TWMNCLButton' +
            'Down);'
          'begin'
          '  inherited;'
          '  //Self.Perform(WM_VSCROLL, SB_LINEDOWN, 0);'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WMNCPAINT(var msgr: TWMNCPaint);'
          'begin'
          '  //inherited;'
          '  Self.PaintNC;'
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsNCScrollMemo.WndProc(var msgr: TMessage);'
          'var'
          '  vsi1, hsi1, vsi2, hsi2: TScrollInfo;'
          '  style: Integer;'
          '  changed: Boolean;'
          'begin'
          
            '  if not HandleAllocated or (msgr.Msg = WM_CREATE) or (msgr.Msg ' +
            '= WM_NCCREATE)'
          '    or (msgr.Msg = WM_DESTROY) or (msgr.Msg = WM_NCDESTROY) then'
          '  begin'
          '    inherited;'
          '    Exit;'
          '  end;'
          ''
          '  changed := False;'
          ''
          '  style := GetWindowLong(Self.Handle, GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi1.cbSize := SizeOf(vsi1);'
          '    vsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi1);'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi1.cbSize := SizeOf(hsi1);'
          '    hsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi1);'
          '  end;'
          '    '
          '  inherited;'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi2.cbSize := SizeOf(vsi2);'
          '    vsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi2);'
          ''
          '    if (vsi1.nMin <> vsi2.nMin) or (vsi1.nMax <> vsi2.nMax)'
          
            '      or (vsi1.nPage <> vsi2.nPage) or (vsi1.nPos <> vsi2.nPos) ' +
            'then'
          '      changed := True;'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi2.cbSize := SizeOf(hsi2);'
          '    hsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi2);'
          ''
          '    if (hsi1.nMin <> hsi2.nMin) or (hsi1.nMax <> hsi2.nMax)'
          
            '      or (hsi1.nPage <> hsi2.nPage) or (hsi1.nPos <> hsi2.nPos) ' +
            'then'
          '      changed := True;'
          '  end;'
          ''
          '  if changed then Self.PaintNC;'
          'end;'
          ''
          '{ TFsGraphicControl }'
          ''
          'procedure TFsGraphicControl.AutoSizeAndInvalidate;'
          'begin'
          '  if not CheckAutoSize then Self.Invalidate;'
          'end;'
          ''
          
            'function TFsGraphicControl.CanAutoSize(var NewWidth, NewHeight: ' +
            'Integer): Boolean;'
          'var'
          '  dim: TSize;'
          'begin'
          '  Result := True;'
          ''
          '  if not (csDestroying in ComponentState) then'
          '  begin'
          '    Self.GetContentDimension(dim);'
          ''
          
            '    if (csDesigning in ComponentState) or (dim.cx > 0) and (dim.' +
            'cy > 0) then'
          '    begin'
          
            '      if Align in [alNone, alLeft, alRight] then NewWidth := dim' +
            '.cx;'
          
            '      if Align in [alNone, alTop, alBottom] then NewHeight := di' +
            'm.cy;'
          '    end;'
          '  end;'
          'end;'
          ''
          'function TFsGraphicControl.CheckAutoSize: Boolean;'
          'var'
          '  OldBounds: TRect;'
          'begin'
          '  if Self.AutoSize then'
          '  begin'
          '    OldBounds := Self.BoundsRect;'
          '    '
          '    Self.AutoSize := False;'
          '    Self.AutoSize := True;'
          ''
          
            '    Result := (OldBounds.Left <> Self.Left) or (OldBounds.Top <>' +
            ' Self.Top)'
          '      or (OldBounds.Right <> Self.Left + Self.Width)'
          '      or (OldBounds.Bottom <> Self.Top + Self.Height);'
          '    '
          '  end'
          '  else Result := False;'
          'end;'
          ''
          'procedure TFsGraphicControl.GetContentDimension(out dim: TSize);'
          'begin'
          '  dim.cx := Self.Width;'
          '  dim.cy := Self.Height;'
          'end;'
          ''
          '{ TFsCoverButton }'
          ''
          'constructor TFsCoverButton.Create(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FPicture := TPicture.Create;'
          '  FPicture.OnChange := Self.PictureChanged;'
          'end;'
          ''
          'destructor TFsCoverButton.Destroy;'
          'begin'
          '  FPicture.Free;'
          '  SetDownCover(nil);'
          '  SetHoverCover(nil);'
          '  inherited;'
          'end;'
          ''
          'procedure TFsCoverButton.DrawPicture(const Rect: TRect);'
          'begin'
          
            '  if Assigned(FPicture.Graphic) and not FPicture.Graphic.Empty t' +
            'hen'
          '    Self.Canvas.StretchDraw(Rect, FPicture.Graphic);'
          'end;'
          ''
          'function TFsCoverButton.GetCover: TFsDrawable;'
          'begin'
          '  inherited;'
          ''
          '  if mfLButtonDown in FMouseFlag then Result := fDownCover'
          '  else if mfMouseOver in FMouseFlag then Result := FHoverCover'
          '  else Result := nil;'
          'end;'
          ''
          
            'function TFsCoverButton.GetPictureSize(out width: Integer): Inte' +
            'ger;'
          'begin'
          
            '  if Assigned(FPicture.Graphic) and not FPicture.Graphic.Empty t' +
            'hen'
          '  begin'
          '    width := FPicture.Graphic.Width;'
          '    Result := FPicture.Graphic.Height;'
          '  end'
          '  else begin'
          '    width := 0;'
          '    Result := 0;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsCoverButton.LinkedPictureChanged(Sender: TObject; I' +
            'D: TNotifyID);'
          'var'
          '  changed: Boolean;'
          'begin'
          '  changed := (Sender = fDownCover) or (Sender = FHoverCover);'
          ''
          '  if ID = niDestroy then'
          '  begin'
          '    if Sender = fDownCover then fDownCover := nil;'
          ''
          '    if Sender = FHoverCover then FHoverCover := nil;'
          '  end;'
          ''
          '  if changed then Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCoverButton.Paint;'
          'var'
          '  cover: TFsDrawable;'
          '  dim: TSize;'
          '  w, h: Integer;'
          '  r: TRect;'
          'begin'
          '  inherited;'
          ''
          '  Self.GetContentDimension(dim);'
          '  cover := Self.GetCover;'
          ''
          '  if Assigned(cover) then'
          '  begin'
          '    w := cover.Width;'
          ''
          '    if (w < dim.cx) and cover.HorzSafeStretch then w := dim.cx;'
          ''
          '    h := cover.Height;'
          ''
          '    if (h < dim.cy) and cover.VertSafeStretch then h := dim.cy;'
          ''
          '    r.Left := (Self.Width - w) div 2;'
          '    r.Right := r.Left + w;'
          '    r.Top := (Self.Height - h) div 2;'
          '    r.Bottom := r.Top + h;'
          ''
          '    if cover is TFsSingleDrawable then'
          '      TFsSingleDrawable(cover).Draw(Self.Canvas, r)'
          '    else if cover is TFsMultiFrameDrawable then'
          
            '      TFsMultiFrameDrawable(cover).DrawFrame(Self.Canvas.Handle,' +
            ' r, 0);'
          '  end;'
          'end;'
          ''
          'procedure TFsCoverButton.PictureChanged(Sender: TObject);'
          'begin'
          '  Self.AutoSizeAndInvalidate;'
          'end;'
          ''
          'procedure TFsCoverButton.SetDownCover(const Value: TFsDrawable);'
          'begin'
          '  if fDownCover <> Value then'
          '  begin'
          '    if Assigned(fDownCover) then'
          
            '      fDownCover.RemoveOnChangeListener(Self.LinkedPictureChange' +
            'd);'
          ''
          '    fDownCover := Value;'
          ''
          '    if Assigned(fDownCover) then'
          '      fDownCover.AddOnChangeListener(Self.LinkedPictureChanged);'
          ''
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsCoverButton.SetHoverCover(const Value: TFsDrawable)' +
            ';'
          'begin'
          '  if FHoverCover <> Value then'
          '  begin'
          '    if Assigned(FHoverCover) then'
          
            '      FHoverCover.RemoveOnChangeListener(Self.LinkedPictureChang' +
            'ed);'
          ''
          '    FHoverCover := Value;'
          ''
          '    if Assigned(FHoverCover) then'
          
            '      FHoverCover.AddOnChangeListener(Self.LinkedPictureChanged)' +
            ';'
          ''
          '    Self.AutoSizeAndInvalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsCoverButton.SetPicture(const Value: TPicture);'
          'begin'
          '  FPicture.Assign(Value);'
          'end;'
          ''
          '{ TFsCustomScrollBar }'
          ''
          'const'
          '  SCROLL_MIN_THUMB_LENGTH = 20;'
          ''
          
            'function TFsCustomScrollBar.CalcHPos(const rc: TRect; const si: ' +
            'TScrollInfo; Y: Integer): Integer;'
          'var'
          '  ThumbWidth, ScrollWidth: Integer;'
          'begin'
          
            '  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) th' +
            'en'
          '  begin'
          '    ScrollWidth := rc.Right - rc.Left - FHArrowWidth shl 1;'
          ''
          
            '    ThumbWidth := si.nPage * ScrollWidth div (si.nMax - si.nMin ' +
            '+ 1);'
          ''
          
            '    if ThumbWidth < Self.MinThumbLength then ThumbWidth := Self.' +
            'MinThumbLength;'
          ''
          '    if Y <= rc.Left + FHArrowWidth then Result := si.nMin'
          
            '    else if Y > rc.Right - FHArrowWidth - ThumbWidth then Result' +
            ' := si.nMax'
          '    else begin'
          '      Y := Y - rc.Left - FHArrowWidth;'
          
            '      Result := (si.nMax - si.nMin + 1 - si.nPage) * Y div (Scro' +
            'llWidth - ThumbWidth);'
          '    end;'
          '  end'
          '  else Result := si.nMin - 1;'
          'end;'
          ''
          
            'procedure TFsCustomScrollBar.CalcHScroll(const rc: TRect; const ' +
            'si: TScrollInfo; var rcLeftArrow, rcRightArrow,'
          '  rcThumb: TRect);'
          'var'
          '  ThumbWidth, ThumbPos, ScrollWidth: Integer;'
          'begin'
          '  rcLeftArrow.Left := rc.Left;'
          '  rcLeftArrow.Right := rc.Left + FHArrowWidth;'
          '  rcLeftArrow.Top := rc.Top;'
          '  rcLeftArrow.Bottom := rc.Bottom;'
          ''
          '  rcRightArrow.Left := rc.Right - FHArrowWidth;'
          '  rcRightArrow.Right := rc.Right;'
          '  rcRightArrow.Top := rc.Top;'
          '  rcRightArrow.Bottom := rc.Bottom;'
          ''
          
            '  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) th' +
            'en'
          '  begin'
          '    ScrollWidth := rc.Right - rc.Left - FHArrowWidth shl 1;'
          ''
          '    rcThumb.Top := rc.Top;'
          '    rcThumb.Bottom := rc.Bottom;'
          ''
          
            '    ThumbWidth := si.nPage * ScrollWidth div (si.nMax - si.nMin ' +
            '+ 1);'
          ''
          
            '    if ThumbWidth < Self.MinThumbLength then ThumbWidth := Self.' +
            'MinThumbLength;'
          ''
          
            '    ThumbPos := si.nPos * (ScrollWidth - ThumbWidth) div (si.nMa' +
            'x - si.nMin + 1 - si.nPage);'
          ''
          '    rcThumb.Left := rc.Left + FHArrowWidth + ThumbPos;'
          '    rcThumb.Right := rcThumb.Left + ThumbWidth;'
          '  end'
          '  else begin'
          '    rcThumb.Left := 0;'
          '    rcThumb.Right := -1;'
          '    rcThumb.Top := 0;'
          '    rcThumb.Bottom := -1;'
          '  end;'
          'end;'
          ''
          
            'function TFsCustomScrollBar.CalcVPos(const rc: TRect; const si: ' +
            'TScrollInfo; Y: Integer): Integer;'
          'var'
          '  ThumbHeight, ScrollHeight: Integer;'
          'begin'
          
            '  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) th' +
            'en'
          '  begin'
          '    ScrollHeight := rc.Bottom - rc.Top - FVArrowHeight shl 1;'
          ''
          
            '    ThumbHeight := si.nPage * ScrollHeight div (si.nMax - si.nMi' +
            'n + 1);'
          ''
          
            '    if ThumbHeight < Self.MinThumbLength then ThumbHeight := Sel' +
            'f.MinThumbLength;'
          ''
          '    if Y <= rc.Top + FVArrowHeight then Result := si.nMin'
          
            '    else if Y > rc.Bottom - FVArrowHeight - ThumbHeight then Res' +
            'ult := si.nMax'
          '    else begin'
          '      Y := Y - rc.Top - FVArrowHeight;'
          
            '      Result := (si.nMax - si.nMin + 1 - si.nPage) * Y div (Scro' +
            'llHeight - ThumbHeight);'
          '    end;'
          '  end'
          '  else Result := si.nMin - 1;'
          'end;'
          ''
          
            'procedure TFsCustomScrollBar.CalcVScroll(const rc: TRect; const ' +
            'si: TScrollInfo;'
          '  var rcTopArrow, rcBottomArrow, rcThumb: TRect);'
          'var'
          '  ThumbHeight, ThumbPos, ScrollHeight: Integer;'
          'begin'
          '  rcTopArrow.Left := rc.Left;'
          '  rcTopArrow.Right := rc.Right;'
          '  rcTopArrow.Top := rc.Top;'
          '  rcTopArrow.Bottom := rcTopArrow.Top + FVArrowHeight;'
          ''
          '  rcBottomArrow.Left := rc.Left;'
          '  rcBottomArrow.Right := rc.Right;'
          '  rcBottomArrow.Top := rc.Bottom - FVArrowHeight;'
          '  rcBottomArrow.Bottom := rc.Bottom;'
          ''
          
            '  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) th' +
            'en'
          '  begin'
          '    ScrollHeight := rc.Bottom - rc.Top - FVArrowHeight shl 1;'
          '    '
          '    rcThumb.Left := rc.Left;'
          '    rcThumb.Right := rc.Right;'
          ''
          
            '    ThumbHeight := si.nPage * ScrollHeight div (si.nMax - si.nMi' +
            'n + 1);'
          ''
          
            '    if ThumbHeight < Self.MinThumbLength then ThumbHeight := Sel' +
            'f.MinThumbLength;'
          '    '
          
            '    ThumbPos := si.nPos * (ScrollHeight - ThumbHeight) div (si.n' +
            'Max - si.nMin + 1 - si.nPage);'
          ''
          '    rcThumb.Top := rc.Top + FVArrowHeight + ThumbPos;'
          '    rcThumb.Bottom := rcThumb.Top + ThumbHeight;'
          '  end'
          '  else begin'
          '    rcThumb.Left := 0;'
          '    rcThumb.Right := -1;'
          '    rcThumb.Top := 0;'
          '    rcThumb.Bottom := -1;'
          '  end;'
          'end;'
          ''
          'procedure TFsCustomScrollBar.Changed;'
          'begin'
          '  '
          'end;'
          ''
          'constructor TFsCustomScrollBar.Create(AOwner: TComponent);'
          'begin'
          '  inherited;'
          '  FVScrollWidth := GetSystemMetrics(SM_CXVSCROLL);'
          '  FHScrollHeight := GetSystemMetrics(SM_CYHSCROLL);'
          '  FVArrowHeight := GetSystemMetrics(SM_CYVSCROLL);'
          '  FHArrowWidth := GetSystemMetrics(SM_CXHSCROLL);'
          '  FMinThumbLength := SCROLL_MIN_THUMB_LENGTH;'
          'end;'
          ''
          
            'procedure TFsCustomScrollBar.SetHArrowWidth(const Value: Integer' +
            ');'
          'begin'
          '  if (FHArrowWidth <> Value) and (Value > 0) then'
          '  begin'
          '    FHArrowWidth := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsCustomScrollBar.SetHScrollHeight(const Value: Integ' +
            'er);'
          'begin'
          '  if (FHScrollHeight <> Value) and (Value > 0) then'
          '  begin'
          '    FHScrollHeight := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsCustomScrollBar.SetMinThumbLength(const Value: Inte' +
            'ger);'
          'begin'
          
            '  if (FMinThumbLength <> Value) and (Value >= SCROLL_MIN_THUMB_L' +
            'ENGTH) then'
          '  begin'
          '    FMinThumbLength := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsCustomScrollBar.SetVArrowHeight(const Value: Intege' +
            'r);'
          'begin'
          '  if (FVArrowHeight <> Value) and (Value > 0) then'
          '  begin'
          '    FVArrowHeight := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsCustomScrollBar.SetVScrollWidth(const Value: Intege' +
            'r);'
          'begin'
          '  if (FVScrollWidth <> Value) and (Value > 0) then'
          '  begin'
          '    FVScrollWidth := Value;'
          '    Changed;'
          '  end;'
          'end;'
          ''
          '{ TFsFlatScrollBar }'
          ''
          
            'procedure TFsFlatScrollBar.DrawHScroll(dc: HDC; const rc, rcLeft' +
            'Arrow, rcRightArrow, rcThumb: TRect);'
          'var'
          '  brush: HBRUSH;'
          'begin'
          '  Windows.FillRect(dc, rc, GetStockObject(LTGRAY_BRUSH));'
          ''
          
            '  GradientFillRect(dc, Rect(rcLeftArrow.Left + 1, rcLeftArrow.To' +
            'p + 1,'
          '    rcLeftArrow.Right - 1, rcLeftArrow.Bottom - 1),'
          '    RGB(255, 255, 255), RGB(229, 229, 229), False);'
          ''
          '  Self.DrawLeftArrow(dc, rcLeftArrow,'
          '    (rcLeftArrow.Right - rcLeftArrow.Left) div 2,'
          '    (rcLeftArrow.Bottom - rcLeftArrow.Top) div 2);'
          ''
          
            '  GradientFillRect(dc, Rect(rcRightArrow.Left + 1, rcRightArrow.' +
            'Top + 1,'
          '    rcRightArrow.Right - 1, rcRightArrow.Bottom - 1),'
          '    RGB(255, 255, 255), RGB(229, 229, 229), False);'
          ''
          '  Self.DrawRightArrow(dc, rcRightArrow,'
          '    (rcRightArrow.Right - rcRightArrow.Left) div 2,'
          '    (rcRightArrow.Bottom - rcRightArrow.Top) div 2);'
          ''
          '  if rcThumb.Left < rcThumb.Right then'
          '  begin'
          '    brush := CreateSolidBrush(RGB(136, 136, 136));'
          ''
          
            '    FillRect(dc, Rect(rcThumb.Left, rcThumb.Top + 1, rcThumb.Rig' +
            'ht - 1, rcThumb.Top + 2), GetStockObject(WHITE_BRUSH));'
          
            '    FillRect(dc, Rect(rcThumb.Left, rcThumb.Top + 2, rcThumb.Lef' +
            't + 1, rcThumb.Bottom - 1), GetStockObject(WHITE_BRUSH));'
          
            '    FillRect(dc, Rect(rcThumb.Right - 1, rcThumb.Top + 1, rcThum' +
            'b.Right, rcThumb.Bottom - 1), brush);'
          
            '    FillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Bottom - 2, rcTh' +
            'umb.Right - 1, rcThumb.Bottom - 1), brush);'
          ''
          '    DeleteObject(brush);'
          ''
          
            '    GradientFillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Top + 2,' +
            ' rcThumb.Right - 1, rcThumb.Bottom - 2),'
          '      RGB(254, 254, 254), RGB(229, 229, 229), True);'
          '  end;'
          'end;'
          ''
          
            'procedure TFsFlatScrollBar.DrawVScroll(dc: HDC; const rc, rcTopA' +
            'rrow, rcBottomArrow, rcThumb: TRect);'
          'var'
          '  brush: HBRUSH;'
          'begin'
          '  Windows.FillRect(dc, rc, GetStockObject(LTGRAY_BRUSH));'
          ''
          
            '  GradientFillRect(dc, Rect(rcTopArrow.Left + 1, rcTopArrow.Top ' +
            '+ 1,'
          '    rcTopArrow.Right - 1, rcTopArrow.Bottom - 1),'
          '    RGB(255, 255, 255), RGB(229, 229, 229), False);'
          ''
          
            '  Self.DrawUpArrow(dc, rcTopArrow, (rcTopArrow.Bottom - rcTopArr' +
            'ow.Top) div 2, (rcTopArrow.Right - rcTopArrow.Left) div 2);'
          ''
          
            '  GradientFillRect(dc, Rect(rcBottomArrow.Left + 1, rcBottomArro' +
            'w.Top + 1,'
          '    rcBottomArrow.Right - 1, rcBottomArrow.Bottom - 1),'
          '    RGB(255, 255, 255), RGB(229, 229, 229), False);'
          ''
          
            '  Self.DrawDownArrow(dc, rcBottomArrow, (rcBottomArrow.Bottom - ' +
            'rcBottomArrow.Top) div 2, (rcBottomArrow.Right - rcBottomArrow.L' +
            'eft) div 2);'
          ''
          '  if rcThumb.Left < rcThumb.Right then'
          '  begin'
          '    brush := CreateSolidBrush(RGB(136, 136, 136));'
          ''
          
            '    FillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Top, rcThumb.Rig' +
            'ht - 2, rcThumb.Top + 1), GetStockObject(WHITE_BRUSH));'
          
            '    FillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Top + 1, rcThumb' +
            '.Left + 2, rcThumb.Bottom), GetStockObject(WHITE_BRUSH));'
          
            '    FillRect(dc, Rect(rcThumb.Right - 2, rcThumb.Top, rcThumb.Ri' +
            'ght - 1, rcThumb.Bottom), brush);'
          
            '    FillRect(dc, Rect(rcThumb.Left + 2, rcThumb.Bottom - 1, rcTh' +
            'umb.Right - 1, rcThumb.Bottom), brush);'
          ''
          '    DeleteObject(brush);'
          ''
          
            '    GradientFillRect(dc, Rect(rcThumb.Left + 2, rcThumb.Top + 1,' +
            ' rcThumb.Right - 2, rcThumb.Bottom - 1),'
          '      RGB(254, 254, 254), RGB(229, 229, 229), False);'
          '  end;'
          'end;'
          ''
          
            'procedure TFsFlatScrollBar.DrawDownArrow(dc: HDC; const rc: TRec' +
            't; h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[0].X := rc.Left + (rc.Right - rc.Left - bw) div 2 ;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top - h) div 2;'
          ''
          '  pts[1].X := pts[0].X + bw;'
          '  pts[1].Y := pts[0].Y;'
          ''
          '  pts[2].X := rc.Left + (rc.Right - rc.Left) div 2;'
          '  pts[2].Y := pts[0].Y + h;'
          ''
          
            '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].' +
            'Y, pts[2].X, pts[2].Y,'
          '    RGB(180, 180, 180), RGB(180, 180, 180), RGB(192, 192, 192));'
          'end;'
          ''
          
            'procedure TFsFlatScrollBar.DrawIntersect(dc: HDC; const rc: TRec' +
            't);'
          'begin'
          '  FillRect(dc, rc, GetStockObject(LTGRAY_BRUSH));'
          'end;'
          ''
          
            'procedure TFsFlatScrollBar.DrawLeftArrow(dc: HDC; const rc: TRec' +
            't; h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[0].X := rc.Left + (rc.Right - rc.Left - h) div 2;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top) div 2;'
          ''
          '  pts[1].X := pts[0].X + h;'
          '  pts[1].Y := rc.Top + (rc.Bottom - rc.Top - bw) div 2;'
          ''
          '  pts[2].X := pts[1].X;'
          '  pts[2].Y := pts[1].Y + bw;'
          ''
          
            '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].' +
            'Y, pts[2].X, pts[2].Y,'
          '    RGB(192, 192, 192), RGB(180, 180, 180), RGB(180, 180, 180));'
          'end;'
          ''
          
            'procedure TFsFlatScrollBar.DrawRightArrow(dc: HDC; const rc: TRe' +
            'ct; h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[1].X := rc.Left + (rc.Right - rc.Left - h) div 2;'
          '  pts[1].Y := rc.Top + (rc.Bottom - rc.Top - bw) div 2;'
          ''
          '  pts[2].X := pts[1].X;'
          '  pts[2].Y := pts[1].Y + bw;'
          ''
          '  pts[0].X := pts[1].X + h;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top) div 2;'
          ''
          
            '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].' +
            'Y, pts[2].X, pts[2].Y,'
          '    RGB(192, 192, 192), RGB(180, 180, 180), RGB(180, 180, 180));'
          'end;'
          ''
          
            'procedure TFsFlatScrollBar.DrawUpArrow(dc: HDC; const rc: TRect;' +
            ' h, bw: Integer);'
          'var'
          '  pts: array [0..2] of TPoint;'
          'begin'
          '  pts[0].X := rc.Left + (rc.Right - rc.Left) div 2;'
          '  pts[0].Y := rc.Top + (rc.Bottom - rc.Top - h) div 2;'
          ''
          '  pts[1].X := rc.Left + (rc.Right - rc.Left - bw) div 2;'
          '  pts[1].Y := pts[0].Y + h;'
          ''
          '  pts[2].X := pts[1].X + bw;'
          '  pts[2].Y := pts[1].Y;'
          ''
          
            '  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].' +
            'Y, pts[2].X, pts[2].Y,'
          '    RGB(192, 192, 192), RGB(180, 180, 180), RGB(180, 180, 180));'
          'end;'
          ''
          '{ TFsScrollContainer }'
          ''
          'procedure TFsScrollContainer.AdjustInnerControlBounds;'
          'var'
          '  L, R, T, B: Integer;'
          '  sb: TFsCustomScrollBar;'
          '  VScroll, HScroll: Boolean;'
          'begin'
          '  if Assigned(FRealControl) then'
          '  begin'
          '  L := 2;'
          '    R := Self.ClientWidth - 2;'
          '    T := 2;'
          '    B := Self.ClientHeight - 2;'
          ''
          '    VScroll := Self.NeedScrollBar(HScroll);'
          ''
          '    sb := Self.GetRealScrollBar;'
          ''
          '    if VScroll then Dec(R, sb.VScrollWidth);'
          ''
          '    if HScroll then Dec(B, sb.HScrollHeight);'
          ''
          '    FRealControl.SetBounds(L, T, R - L, B - T);'
          '  end;'
          'end;'
          ''
          'procedure TFsScrollContainer.CMMouseEnter(var msgr: TMessage);'
          'begin'
          '  inherited;'
          '  FMouseInControl := True;'
          '  Self.Invalidate;'
          'end;'
          ''
          'procedure TFsScrollContainer.CMMouseLeave(var msgr: TMessage);'
          'begin'
          '  inherited;'
          '  FMouseInControl := False;'
          '  Self.Invalidate;'
          'end;'
          ''
          
            'function TFsScrollContainer.ControlMessage(msg: DWORD; wparam, l' +
            'param: Integer): Integer;'
          'begin'
          
            '  if Assigned(FRealControl) then Result := FRealControl.Perform(' +
            'msg, wparam, lparam)'
          '  else Result := -1;'
          'end;'
          ''
          'constructor TFsScrollContainer.Create(AOwner: TComponent);'
          'begin'
          '  inherited;'
          ''
          
            '  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEve' +
            'nts,'
          
            '    csSetCaption, csOpaque, csDoubleClicks, csReplicatable, csPa' +
            'nnable];'
          ''
          '  Width := 185;'
          '  Height := 41;'
          ''
          '  FRealControl := Self.CreateRealControl;'
          '  FRealControl.Parent := Self;'
          ''
          '  FTimer := TTimer.Create(Self);'
          '  FTimer.Enabled := False;'
          '  FTimer.Interval := 200;'
          '  FTimer.OnTimer := Self.OnTimer;'
          'end;'
          ''
          
            'procedure TFsScrollContainer.CreateParams(var Params: TCreatePar' +
            'ams);'
          'begin'
          '  inherited;'
          ''
          '  Params.Style := Params.Style and not WS_BORDER;'
          
            '  Params.WindowClass.style := Params.WindowClass.style or CS_HRE' +
            'DRAW or CS_VREDRAW;'
          'end;'
          ''
          'destructor TFsScrollContainer.Destroy;'
          'begin'
          ''
          '  inherited;'
          'end;'
          ''
          
            'function TFsScrollContainer.GetControlScrollInfo(var si: TScroll' +
            'Info; isVert: Boolean): Boolean;'
          'const'
          '  BARS: array [Boolean] of DWORD = (SB_HORZ, SB_VERT);'
          'begin'
          
            '  if Assigned(FRealControl) and (FRealControl is TWinControl) an' +
            'd (TWinControl(FRealControl).HandleAllocated) then'
          
            '    Result := Windows.GetScrollInfo(TWinControl(FRealControl).Ha' +
            'ndle, BARS[isVert], si) and (si.nPage <> 0)'
          '  else Result := False;'
          'end;'
          ''
          
            'function TFsScrollContainer.GetRealScrollBar: TFsCustomScrollBar' +
            ';'
          'begin'
          '  if Assigned(FScrollBarDrawer) then Result := FScrollBarDrawer'
          '  else Result := GetDefaultScrollBar;'
          'end;'
          ''
          
            'procedure TFsScrollContainer.GetScrollInfo(var fsi: TFsScrollInf' +
            'o; var vsi, hsi: TScrollInfo);'
          'var'
          '  sb: TFsCustomScrollBar;'
          'begin'
          '  sb := GetRealScrollBar;'
          ''
          '  fsi.ShowVScroll := False;'
          '  fsi.ShowHScroll := False;'
          ''
          '  vsi.cbSize := SizeOf(vsi);'
          '  vsi.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;'
          ''
          
            '  if GetControlScrollInfo(vsi, True) and (vsi.nMax - vsi.nMin + ' +
            '1 > vsi.nPage) then'
          '    fsi.ShowVScroll := True;'
          ''
          '  hsi.cbSize := SizeOf(hsi);'
          '  hsi.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;'
          ''
          
            '  if GetControlScrollInfo(hsi, False) and (hsi.nMax - hsi.nMin +' +
            ' 1 > hsi.nPage) then'
          '    fsi.ShowHScroll := True;'
          ''
          '  if fsi.ShowVScroll then'
          '  begin'
          '    fsi.VScroll.Right := Self.Width - 2;'
          '    fsi.VScroll.Left := fsi.VScroll.Right - sb.VScrollWidth;'
          '    fsi.VScroll.Top := 2;'
          ''
          
            '    if fsi.ShowHScroll then fsi.VScroll.Bottom := Self.Height - ' +
            '2 - sb.HScrollHeight'
          '    else fsi.VScroll.Bottom := Self.Height - 2;'
          ''
          
            '    sb.CalcVScroll(fsi.VScroll, vsi, fsi.TopArrow, fsi.BottomArr' +
            'ow, fsi.VThumb);'
          '  end;'
          ''
          '  if fsi.ShowHScroll then'
          '  begin'
          '    fsi.HScroll.Bottom := Self.Height - 2;'
          '    fsi.HScroll.Top := fsi.HScroll.Bottom - sb.HScrollHeight;'
          '    fsi.HScroll.Left := 2;'
          ''
          
            '    if fsi.ShowHScroll then fsi.HScroll.Right := Self.Width - 2 ' +
            '- sb.VScrollWidth'
          '    else fsi.HScroll.Right := Self.Width - 2;'
          ''
          
            '    sb.CalcHScroll(fsi.HScroll, hsi, fsi.LeftArrow, fsi.RightArr' +
            'ow, fsi.HThumb);'
          '  end;'
          ''
          '  if fsi.ShowVScroll and fsi.ShowHScroll then'
          '  begin'
          '    fsi.Intersect.Left := Self.Width - 2 - sb.VScrollWidth;'
          '    fsi.Intersect.Right := Self.Width - 2;'
          '    fsi.Intersect.Top := Self.Height - 2 - sb.HScrollHeight;'
          '    fsi.Intersect.Bottom := Self.Height - 2;'
          '  end;'
          'end;'
          ''
          
            'function TFsScrollContainer.HitTest(X, Y: Integer): TScrollHitTe' +
            'st;'
          'var'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          '  pt: TPoint;'
          'begin'
          '  pt.X := X;'
          '  pt.Y := Y;'
          ''
          '  Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '  Result := shtNoWhere;'
          ''
          '  if fsi.ShowVScroll then'
          '  begin'
          '    if PtInRect(fsi.TopArrow, pt) then Result := shtTopArrow'
          
            '    else if PtInRect(fsi.BottomArrow, pt) then Result := shtBott' +
            'omArrow'
          '    else if PtInRect(fsi.VThumb, pt) then Result := shtVertThumb'
          '    else if PtInRect(fsi.VScroll, pt) then'
          '    begin'
          '      if pt.Y < fsi.VThumb.Top then Result := shtPageUp'
          '      else Result := shtPageDown;'
          '    end;'
          '  end;'
          ''
          '  if fsi.ShowHScroll and (Result = shtNoWhere) then'
          '  begin'
          '    if PtInRect(fsi.LeftArrow, pt) then Result := shtLeftArrow'
          
            '    else if PtInRect(fsi.RightArrow, pt) then Result := shtRight' +
            'Arrow'
          '    else if PtInRect(fsi.HThumb, pt) then Result := shtHorzThumb'
          '    else if PtInRect(fsi.HScroll, pt) then'
          '    begin'
          '      if pt.X < fsi.HThumb.Left then Result := shtPageLeft'
          '      else Result := shtPageRight;'
          '    end;'
          '  end;'
          'end;'
          ''
          
            'procedure TFsScrollContainer.MouseDown(Button: TMouseButton; Shi' +
            'ft: TShiftState; X, Y: Integer);'
          'var'
          '  sht: TScrollHitTest;'
          '  DoCapture: Boolean;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          'begin'
          '  DoCapture := True;'
          '  sht := Self.HitTest(X, Y);'
          ''
          '  case sht of'
          '    shtNoWhere, shtBorder: DoCapture := False;'
          
            '    shtLeftArrow: Self.ControlMessage(WM_HSCROLL, SB_LINELEFT, 0' +
            ');'
          
            '    shtRightArrow: Self.ControlMessage(WM_HSCROLL, SB_LINERIGHT,' +
            ' 0);'
          '    shtHorzThumb: ;'
          
            '    shtPageLeft: Self.ControlMessage(WM_HSCROLL, SB_PAGELEFT, 0)' +
            ';'
          
            '    shtPageRight: Self.ControlMessage(WM_HSCROLL, SB_PAGERIGHT, ' +
            '0);'
          '    shtTopArrow: Self.ControlMessage(WM_VSCROLL, SB_LINEUP, 0);'
          
            '    shtBottomArrow: Self.ControlMessage(WM_VSCROLL, SB_LINEDOWN,' +
            ' 0);'
          '    shtVertThumb: ;'
          '    shtPageUp: Self.ControlMessage(WM_VSCROLL, SB_PAGEUP, 0);'
          
            '    shtPageDown: Self.ControlMessage(WM_VSCROLL, SB_PAGEDOWN, 0)' +
            ';'
          '  end;'
          ''
          '  if DoCapture then'
          '  begin'
          '    FCaptureRegion := sht;'
          '    SetCapture(Self.Handle);'
          ''
          '    if FCaptureRegion in [shtVertThumb, shtHorzThumb] then'
          '    begin'
          '      Self.GetScrollInfo(fsi, vsi, hsi);'
          '      FCapturePoint.X := X - fsi.HThumb.Left;'
          '      FCapturePoint.Y := Y - fsi.VThumb.Top;'
          '    end'
          '    else FTimer.Enabled := True;'
          '  end;'
          ''
          '  inherited;'
          'end;'
          ''
          
            'procedure TFsScrollContainer.MouseMove(Shift: TShiftState; X, Y:' +
            ' Integer);'
          'var'
          '  pt: TPoint;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          '  nPos: Integer;'
          'begin'
          '  case FCaptureRegion of'
          '    shtLeftArrow, shtRightArrow, shtPageLeft, shtPageRight,'
          '    shtTopArrow, shtBottomArrow, shtPageUp, shtPageDown:'
          '      begin'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          '        FTimer.Enabled := HitTest(pt.X, pt.Y) = FCaptureRegion;'
          '      end;'
          ''
          '    shtVertThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowVScroll then'
          '        begin'
          
            '          nPos := GetRealScrollBar.CalcVPos(fsi.VScroll, vsi, Y ' +
            '- FCapturePoint.Y);'
          ''
          '          if nPos >= hsi.nMin then'
          
            '            Self.ControlMessage(WM_VSCROLL, MakeLong(SB_THUMBTRA' +
            'CK, nPos),0);'
          '        end;'
          '      end;'
          ''
          '    shtHorzThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowHScroll then'
          '        begin'
          
            '          nPos := GetRealScrollBar.CalcHPos(fsi.HScroll, hsi, X ' +
            '- FCapturePoint.X);'
          ''
          '          if nPos >= hsi.nMin then'
          
            '            Self.ControlMessage(WM_HSCROLL, MakeLong(SB_THUMBTRA' +
            'CK, nPos),0);'
          '        end;'
          '      end;'
          '  end;'
          ''
          '  inherited;'
          'end;'
          ''
          
            'procedure TFsScrollContainer.MouseUp(Button: TMouseButton; Shift' +
            ': TShiftState; X, Y: Integer);'
          'var'
          '  pt: TPoint;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          '  nPos: Integer;'
          'begin'
          '  ReleaseCapture;'
          '  FCaptureRegion := shtNoWhere;'
          '  FTimer.Enabled := False;'
          ''
          '  case FCaptureRegion of'
          '    shtVertThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowHScroll then'
          '        begin'
          
            '          nPos := GetRealScrollBar.CalcVPos(fsi.VScroll, vsi, Y ' +
            '- FCapturePoint.Y);'
          ''
          '          if nPos >= hsi.nMin then'
          
            '            Self.ControlMessage(WM_VSCROLL, MakeLong(SB_THUMBPOS' +
            'ITION, nPos),0);'
          '        end;'
          '      end;'
          ''
          '    shtHorzThumb:'
          '      begin'
          '        Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '        if fsi.ShowHScroll then'
          '        begin'
          
            '          nPos := GetRealScrollBar.CalcHPos(fsi.HScroll, hsi, X ' +
            '- FCapturePoint.X);'
          ''
          '          if nPos >= hsi.nMin then'
          
            '            Self.ControlMessage(WM_HSCROLL, MakeLong(SB_THUMBPOS' +
            'ITION, nPos),0);'
          '        end;'
          '      end;'
          '  end;'
          '  '
          '  inherited;'
          'end;'
          ''
          
            'function TFsScrollContainer.NeedScrollBar(out HScroll: Boolean):' +
            ' Boolean;'
          'var'
          '  style: Integer;'
          '  si: TScrollInfo;'
          'begin'
          '  HScroll := False;'
          '  Result := False;'
          ''
          '  si.cbSize := SizeOf(si);'
          '  si.fMask := SIF_RANGE or SIF_PAGE;'
          ''
          
            '  HScroll := GetControlScrollInfo(si, False) and (si.nMax - si.n' +
            'Min + 1 > si.nPage);'
          ''
          '  si.cbSize := SizeOf(si);'
          '  si.fMask := SIF_RANGE or SIF_PAGE;'
          ''
          
            '  Result := GetControlScrollInfo(si, True) and (si.nMax - si.nMi' +
            'n + 1 > si.nPage);'
          'end;'
          ''
          'procedure TFsScrollContainer.OnTimer(Sender: TObject);'
          'var'
          '  pt: TPoint;'
          'begin'
          '  case FCaptureRegion of'
          
            '    shtLeftArrow: Self.ControlMessage(WM_HSCROLL, SB_LINELEFT, 0' +
            ');'
          
            '    shtRightArrow: Self.ControlMessage(WM_HSCROLL, SB_LINERIGHT,' +
            ' 0);'
          '    shtPageLeft:'
          '      begin'
          '        Self.ControlMessage(WM_HSCROLL, SB_PAGELEFT, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          
            '        if HitTest(pt.X, pt.Y) <> shtPageLeft then FTimer.Enable' +
            'd := False;'
          '      end;'
          '    shtPageRight:'
          '      begin'
          '        Self.ControlMessage(WM_HSCROLL, SB_PAGERIGHT, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          
            '        if HitTest(pt.X, pt.Y) <> shtPageLeft then FTimer.Enable' +
            'd := False;'
          '      end;'
          '    shtTopArrow: Self.ControlMessage(WM_VSCROLL, SB_LINEUP, 0);'
          
            '    shtBottomArrow: Self.ControlMessage(WM_VSCROLL, SB_LINEDOWN,' +
            ' 0);'
          '    shtPageUp:'
          '      begin'
          '        Self.ControlMessage(WM_VSCROLL, SB_PAGEUP, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          
            '        if HitTest(pt.X, pt.Y) <> shtPageUp then FTimer.Enabled ' +
            ':= False;'
          '      end;'
          '    shtPageDown:'
          '      begin'
          '        Self.ControlMessage(WM_VSCROLL, SB_PAGEDOWN, 0);'
          '        GetCursorPos(pt);'
          '        Windows.ScreenToClient(Handle, pt);'
          
            '        if HitTest(pt.X, pt.Y) <> shtPageDown then FTimer.Enable' +
            'd := False;'
          '      end;'
          '  end;'
          'end;'
          ''
          'procedure TFsScrollContainer.Paint;'
          'var'
          '  sb: TFsCustomScrollBar;'
          '  r: TRect;'
          '  fsi: TFsScrollInfo;'
          '  vsi, hsi: TScrollInfo;'
          'begin'
          '  inherited;'
          ''
          
            '  if FMouseInControl then Canvas.Brush.Color := RGB(123, 228, 25' +
            '5)'
          '  else Canvas.Brush.Color := RGB(78, 160, 209);'
          ''
          '  r.Left := 0;'
          '  r.Top := 0;'
          '  r.Right := Self.Width;'
          '  r.Bottom := Self.Height;'
          ''
          '  Canvas.FrameRect(r);'
          ''
          
            '  if FMouseInControl then Canvas.Brush.Color := RGB(78, 160, 209' +
            ')'
          '  else Canvas.Brush.Color := TControlHack(FRealControl).Color;'
          ''
          '  r.Left := 1;'
          '  r.Top := 1;'
          '  r.Right := Self.Width - 1;'
          '  r.Bottom := Self.Height - 1;'
          ''
          '  Canvas.FrameRect(r);'
          ''
          '  sb := GetRealScrollBar;'
          ''
          '  Self.GetScrollInfo(fsi, vsi, hsi);'
          ''
          '  if fsi.ShowVScroll then'
          
            '    sb.DrawVScroll(Canvas.Handle, fsi.VScroll, fsi.TopArrow, fsi' +
            '.BottomArrow, fsi.VThumb);'
          ''
          '  if fsi.ShowHScroll then'
          
            '    sb.DrawHScroll(Canvas.Handle, fsi.HScroll, fsi.LeftArrow, fs' +
            'i.RightArrow, fsi.HThumb);'
          ''
          '  if fsi.ShowVScroll and fsi.ShowHScroll then'
          '    sb.DrawIntersect(Canvas.Handle, fsi.Intersect);'
          'end;'
          ''
          
            'procedure TFsScrollContainer.SetScrollBarDrawer(const Value: TFs' +
            'CustomScrollBar);'
          'begin'
          '  if Value <> FScrollBarDrawer then'
          '  begin'
          '    if Assigned(FScrollBarDrawer) then'
          '      FScrollBarDrawer.RemoveFreeNotification(Self);'
          ''
          '    FScrollBarDrawer := Value;'
          ''
          '    if Assigned(FScrollBarDrawer) then'
          '      FScrollBarDrawer.FreeNotification(Self);'
          ''
          '    Self.AdjustInnerControlBounds;'
          ''
          '    //Self.Invalidate;'
          '  end;'
          'end;'
          ''
          'procedure TFsScrollContainer.WMSize(var msgr: TWMSize);'
          'begin'
          '  inherited;'
          '  AdjustInnerControlBounds;'
          'end;'
          ''
          '{ TFsMemo }'
          ''
          'function TFsMemo.CreateRealControl: TControl;'
          'begin'
          '  Result := TFsBorderlessMemo.Create(Self);'
          '  TFsBorderlessMemo(Result).ScrollBars := ssVertical;'
          'end;'
          ''
          'function TFsMemo.GetLines: TStrings;'
          'begin'
          '  Result := TFsBorderlessMemo(RealControl).Lines;'
          'end;'
          ''
          'function TFsMemo.GetScrollBars: TScrollStyle;'
          'begin'
          '  Result := TFsBorderlessMemo(RealControl).ScrollBars;'
          'end;'
          ''
          'procedure TFsMemo.SetLines(const Value: TStrings);'
          'begin'
          '  TFsBorderlessMemo(RealControl).Lines := Value;'
          'end;'
          ''
          'procedure TFsMemo.SetScrollBars(const Value: TScrollStyle);'
          'begin'
          '  TFsBorderlessMemo(RealControl).ScrollBars := Value;'
          'end;'
          ''
          '{ TFsBorderlessMemo }'
          ''
          
            'procedure TFsBorderlessMemo.WMNCCalcSize(var msgr: TWMNCCalcSize' +
            ');'
          'begin'
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsBorderlessMemo.WMNCPaint(var msgr: TWMNCPaint);'
          'begin'
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsBorderlessMemo.WndProc(var msgr: TMessage);'
          'var'
          '  vsi1, hsi1, vsi2, hsi2: TScrollInfo;'
          '  style: Integer;'
          '  changed: Boolean;'
          'begin'
          
            '  if not HandleAllocated or (msgr.Msg = WM_CREATE) or (msgr.Msg ' +
            '= WM_NCCREATE)'
          '    or (msgr.Msg = WM_DESTROY) or (msgr.Msg = WM_NCDESTROY) then'
          '  begin'
          '    inherited;'
          '    Exit;'
          '  end;'
          ''
          '  changed := False;'
          ''
          '  style := GetWindowLong(Self.Handle, GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi1.cbSize := SizeOf(vsi1);'
          '    vsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi1);'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi1.cbSize := SizeOf(hsi1);'
          '    hsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi1);'
          '  end;'
          ''
          '  inherited;'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi2.cbSize := SizeOf(vsi2);'
          '    vsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi2);'
          ''
          '    if (vsi1.nMin <> vsi2.nMin) or (vsi1.nMax <> vsi2.nMax)'
          
            '      or (vsi1.nPage <> vsi2.nPage) or (vsi1.nPos <> vsi2.nPos) ' +
            'then'
          '      changed := True;'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi2.cbSize := SizeOf(hsi2);'
          '    hsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi2);'
          ''
          '    if (hsi1.nMin <> hsi2.nMin) or (hsi1.nMax <> hsi2.nMax)'
          
            '      or (hsi1.nPage <> hsi2.nPage) or (hsi1.nPos <> hsi2.nPos) ' +
            'then'
          '      changed := True;'
          '  end;'
          ''
          '  if changed and (Self.Parent is TFsScrollContainer) then'
          '  begin'
          '    TFsScrollContainer(Self.Parent).AdjustInnerControlBounds;'
          '    Self.Parent.Invalidate;'
          '  end;'
          'end;'
          ''
          '{ TFsListBox }'
          ''
          'function TFsListBox.CreateRealControl: TControl;'
          'begin'
          '  Result := TFsBorderlessListBox.Create(Self);'
          'end;'
          ''
          'function TFsListBox.GetItems: TStrings;'
          'begin'
          '  Result := TFsBorderlessListBox(RealControl).Items;'
          'end;'
          ''
          'procedure TFsListBox.SetItems(const Value: TStrings);'
          'begin'
          '  TFsBorderlessListBox(RealControl).Items := Value;'
          'end;'
          ''
          '{ TFsBorderlessListBox }'
          ''
          
            'procedure TFsBorderlessListBox.WMNCCalcSize(var msgr: TWMNCCalcS' +
            'ize);'
          'begin'
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsBorderlessListBox.WMNCPaint(var msgr: TWMNCPaint);'
          'begin'
          '  msgr.Result := 0;'
          'end;'
          ''
          'procedure TFsBorderlessListBox.WndProc(var msgr: TMessage);'
          'var'
          '  vsi1, hsi1, vsi2, hsi2: TScrollInfo;'
          '  style: Integer;'
          '  changed: Boolean;'
          'begin'
          
            '  if not HandleAllocated or (msgr.Msg = WM_CREATE) or (msgr.Msg ' +
            '= WM_NCCREATE)'
          '    or (msgr.Msg = WM_DESTROY) or (msgr.Msg = WM_NCDESTROY) then'
          '  begin'
          '    inherited;'
          '    Exit;'
          '  end;'
          ''
          '  changed := False;'
          ''
          '  style := GetWindowLong(Self.Handle, GWL_STYLE);'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi1.cbSize := SizeOf(vsi1);'
          '    vsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi1);'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi1.cbSize := SizeOf(hsi1);'
          '    hsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi1);'
          '  end;'
          ''
          '  inherited;'
          ''
          '  if style and WS_VSCROLL <> 0 then'
          '  begin'
          '    vsi2.cbSize := SizeOf(vsi2);'
          '    vsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_VERT, vsi2);'
          ''
          '    if (vsi1.nMin <> vsi2.nMin) or (vsi1.nMax <> vsi2.nMax)'
          
            '      or (vsi1.nPage <> vsi2.nPage) or (vsi1.nPos <> vsi2.nPos) ' +
            'then'
          '      changed := True;'
          '  end;'
          ''
          '  if style and WS_HSCROLL <> 0 then'
          '  begin'
          '    hsi2.cbSize := SizeOf(hsi2);'
          '    hsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;'
          '    GetScrollInfo(Self.Handle, SB_HORZ, hsi2);'
          ''
          '    if (hsi1.nMin <> hsi2.nMin) or (hsi1.nMax <> hsi2.nMax)'
          
            '      or (hsi1.nPage <> hsi2.nPage) or (hsi1.nPos <> hsi2.nPos) ' +
            'then'
          '      changed := True;'
          '  end;'
          ''
          '  if changed and (Self.Parent is TFsScrollContainer) then'
          '  begin'
          '    TFsScrollContainer(Self.Parent).AdjustInnerControlBounds;'
          '    Self.Parent.Invalidate;'
          '  end;'
          'end;'
          ''
          'initialization'
          ''
          'finalization'
          '  DefaultScrollBar.Free;'
          '  '
          'end.')
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'ListView'
      ImageIndex = 2
      object FsListView1: TFsListView
        Left = 0
        Top = 0
        Width = 850
        Height = 509
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvRaised
        BevelKind = bkFlat
        ParentBackground = False
        ScrollBarDrawer = FsFlatScrollBar1
        Columns = <>
        ViewStyle = vsReport
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TreeView'
      ImageIndex = 3
      object FsTreeView1: TFsTreeView
        Left = 0
        Top = 0
        Width = 850
        Height = 509
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvRaised
        BevelKind = bkFlat
        ParentBackground = False
        ScrollBarDrawer = FsFlatScrollBar1
      end
    end
  end
  object npdMinButton: TFsNinePitchDrawable
    Graphic.Data = {
      621F0000424D621F00000000000036000000280000007E000000150000000100
      1800000000002C1F000000000000000000000000000000000000874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A207D451E7D451E7D
      451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E
      7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E8F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57307D451E7D451E7D451E7D
      451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E
      7D451E7D451E7D451E7D451E7D451E7D451E7D451E8F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57300000874A20C99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6E874A207D451EBB8F66BB8F66BB8F
      66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB
      8F66BB8F66BB8F66BB8F66BB8F66BB8F667D451E8F5730CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA1788F57308F5730CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA1788F57307D451EBB8F66BB8F66BB8F66BB8F
      66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB
      8F66BB8F66BB8F66BB8F66BB8F667D451E8F5730CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA1788F57300000874A20C99A6E9F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40CDA1788F57300000874A20C99A6E9F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E94
      5B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2EBB
      8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1
      788F57300000874A20C99A6E9F62329F62329F6232FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF9F62329F62329F62329F62329F62329F62329F6232C9
      9A6E874A207D451EBB8F66945B2E945B2E945B2ED8A278D8A278D8A278D8A278
      D8A278D8A278D8A278945B2E945B2E945B2E945B2E945B2E945B2E945B2EBB8F
      667D451E8F5730CDA178A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFA66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA178
      8F57308F5730CDA178A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFA66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F
      57307D451EBB8F66945B2E945B2E945B2EE2B097E2B097E2B097E2B097E2B097
      E2B097E2B097945B2E945B2E945B2E945B2E945B2E945B2E945B2EBB8F667D45
      1E8F5730CDA178A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFA66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F5730
      0000874A20C99A6E9F62329F62329F6232FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF9F62329F62329F62329F62329F62329F62329F6232C99A6E874A
      207D451EBB8F66945B2E945B2E945B2ED8A278D8A278D8A278D8A278D8A278D8
      A278D8A278945B2E945B2E945B2E945B2E945B2E945B2E945B2EBB8F667D451E
      8F5730CDA178A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFA66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F57308F
      5730CDA178A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFA66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F57307D45
      1EBB8F66945B2E945B2E945B2EE2B097E2B097E2B097E2B097E2B097E2B097E2
      B097945B2E945B2E945B2E945B2E945B2E945B2E945B2EBB8F667D451E8F5730
      CDA178A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFA66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F57300000874A
      20C99A6E9F62329F62329F6232FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF9F62329F62329F62329F62329F62329F62329F6232C99A6E874A207D451E
      BB8F66945B2E945B2E945B2ED8A278D8A278D8A278D8A278D8A278D8A278D8A2
      78945B2E945B2E945B2E945B2E945B2E945B2E945B2EBB8F667D451E8F5730CD
      A178A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F57308F5730CDA1
      78A66D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F57307D451EBB8F66
      945B2E945B2E945B2EE2B097E2B097E2B097E2B097E2B097E2B097E2B097945B
      2E945B2E945B2E945B2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A6
      6D40A66D40A66D40FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA66D40
      A66D40A66D40A66D40A66D40A66D40A66D40CDA1788F57300000874A20C99A6E
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F6232C99A6E874A207D451EBB8F6694
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F6232C99A6E874A207D451EBB8F66945B2E945B
      2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B
      2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40CDA1788F57300000874A20CF9F72A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534CF9F72874A207D451EC0946A985E30985E30985E30985E30985E30985E
      30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E3098
      5E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42D2A67C8F57308F5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      D2A67C8F57307D451EC0946A985E30985E30985E30985E30985E30985E30985E
      30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30C0
      946A7D451E8F5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A6
      7C8F57300000874A20CF9F72A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534CF
      9F72874A207D451EC0946A985E30985E30985E30985E30985E30985E30985E30
      985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30C094
      6A7D451E8F5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C
      8F57308F5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F
      57307D451EC0946A985E30985E30985E30985E30985E30985E30985E30985E30
      985E30985E30985E30985E30985E30985E30985E30985E30985E30C0946A7D45
      1E8F5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F5730
      0000874A20CF9F72A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534CF9F72874A
      207D451EC0946A985E30985E30985E30985E30985E30985E30985E30985E3098
      5E30985E30985E30985E30985E30985E30985E30985E30985E30C0946A7D451E
      8F5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57308F
      5730D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57307D45
      1EC0946A985E30985E30985E30985E30985E30985E30985E30985E30985E3098
      5E30985E30985E30985E30985E30985E30985E30985E30C0946A7D451E8F5730
      D2A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57300000874A
      20CF9F72A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534CF9F72874A207D451E
      C0946A985E30985E30985E30985E30985E30985E30985E30985E30985E30985E
      30985E30985E30985E30985E30985E30985E30985E30C0946A7D451E8F5730D2
      A67CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57308F5730D2A6
      7CAA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57307D451EC0946A
      985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E
      30985E30985E30985E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57300000874A20CF9F72
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534CF9F72874A207D451EC0946A98
      5E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30
      985E30985E30985E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57308F5730D2A67CAA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042D2A67C8F57307D451EC0946A985E3098
      5E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30
      985E30985E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042D2A67C8F57300000874A20CF9F72A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534CF9F72874A207D451EC0946A985E30985E
      30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E3098
      5E30985E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042D2A67C8F57308F5730D2A67CAA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042D2A67C8F57307D451EC0946A985E30985E30985E
      30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E3098
      5E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042D2A67C8F57300000874A20CF9F72A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534CF9F72874A207D451EC0946A985E30985E30985E30
      985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E
      30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042D2A67C8F57308F5730D2A67CAA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042D2A67C8F57307D451EC0946A985E30985E30985E30985E30
      985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E
      30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042D2A67C8F57300000874A20CF9F72CF9F72CF9F72CF9F72CF9F72
      CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F
      72CF9F72CF9F72CF9F72874A207D451EC0946AC0946AC0946AC0946AC0946AC0
      946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946A
      C0946AC0946AC0946A7D451E8F5730D2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67CD2A67C8F57308F5730D2A67CD2A67CD2A67CD2A67CD2A67CD2A67C
      D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67C8F57307D451EC0946AC0946AC0946AC0946AC0946AC0946AC0
      946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946A
      C0946AC0946A7D451E8F5730D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67C8F57300000874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A207D451E7D451E7D451E7D451E7D451E7D451E7D451E7D45
      1E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D
      451E7D451E7D451E8F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57307D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D45
      1E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D
      451E7D451E8F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57300000}
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 0
    MarginBottom = 0
    Transparent = False
    TransparentColor = 50266367
    RowCount = 1
    ColCount = 6
    Left = 32
    Top = 24
  end
  object npdTopBorder: TFsNinePitchDrawable
    Graphic.Data = {
      2E610000424D2E6100000000000036000000280000008E0000003A0000000100
      180000000000F8600000000000000000000000000000000000008F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57300000CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA1780000A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D400000A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D400000A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D400000A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      0000A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D400000A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D400000A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D400000A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D400000A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D400000A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D400000A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D400000A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      0000A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D400000AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70420000AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA70420000AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA70420000AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70420000AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA70420000AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA70420000AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      0000AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA70420000AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70420000AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA70420000AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA70420000AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70420000D2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67C
      D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67C
      D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67C
      D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67C
      D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67CD2A67C00008F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57300000874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      0000C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6E00009F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F623200009F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F623200009F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F623200009F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F623200009F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F623200009F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F623200009F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      00009F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F623200009F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F623200009F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F623200009F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F623200009F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F623200009F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62320000A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A465340000A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      0000A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A465340000A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A465340000A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A465340000A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A465340000A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A465340000A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A465340000A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A465340000A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      0000A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A465340000A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534A46534A46534A465340000CF9F72CF9F72
      CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F
      72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF
      9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72
      CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F
      72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF
      9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72
      CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F
      72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF
      9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72
      CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F
      72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF
      9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72
      CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F
      72CF9F720000874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A200000}
    MarginLeft = 7
    MarginRight = 7
    MarginTop = 0
    MarginBottom = 0
    Transparent = False
    TransparentColor = 50266367
    RowCount = 2
    ColCount = 1
    Left = 176
    Top = 24
  end
  object npdClose: TFsNinePitchDrawable
    Graphic.Data = {
      621F0000424D621F00000000000036000000280000007E000000150000000100
      1800000000002C1F0000000000000000000000000000000000000000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A400000000A42929EF2929EF29
      29EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF
      2929EF2929EF2929EF2929EF2929EF2929EF0000A40000A42626DE2626DE2626
      DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE26
      26DE2626DE2626DE2626DE2626DE2626DE0000A40000A43838F03838F03838F0
      3838F03838F03838F03838F03838F03838F03838F03838F03838F03838F03838
      F03838F03838F03838F03838F03838F00000A40000A43838F03838F03838F038
      38F03838F03838F03838F03838F03838F03838F03838F03838F03838F03838F0
      3838F03838F03838F03838F03838F00000A40000A42626DE2626DE2626DE2626
      DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE2626DE26
      26DE2626DE2626DE2626DE2626DE0000A40000A43838F03838F03838F03838F0
      3838F03838F03838F03838F03838F03838F03838F03838F03838F03838F03838
      F03838F03838F03838F03838F00000A400000000A42929EF0000CC0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC2929EF0000A40000A42626DE0000BE0000BE0000BE
      0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000
      BE0000BE0000BE0000BE2626DE0000A40000A43838F01212D01212D01212D012
      12D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212D0
      1212D01212D01212D03838F00000A40000A43838F01212D01212D01212D01212
      D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212D012
      12D01212D01212D03838F00000A40000A42626DE0000BE0000BE0000BE0000BE
      0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000
      BE0000BE0000BE2626DE0000A40000A43838F01212D01212D01212D01212D012
      12D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212D0
      1212D01212D03838F00000A400000000A42929EF0000CC0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000
      CC0000CC0000CC2929EF0000A40000A42626DE0000BE0000BE0000BE0000BE00
      00BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE
      0000BE0000BE2626DE0000A40000A43838F01212D01212D01212D01212D01212
      D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212D012
      12D01212D03838F00000A40000A43838F01212D01212D01212D01212D01212D0
      1212D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212
      D01212D03838F00000A40000A42626DE0000BE0000BE0000BE0000BE0000BE00
      00BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE
      0000BE2626DE0000A40000A43838F01212D01212D01212D01212D01212D01212
      D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212D012
      12D03838F00000A400000000A42929EF0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC
      0000CC2929EF0000A40000A42626DE0000BE0000BE0000BE0000BE0000BE0000
      BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE00
      00BE2626DE0000A40000A43838F01212D01212D01212D01212D01212D01212D0
      1212D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212
      D03838F00000A40000A43838F01212D01212D01212D01212D01212D01212D012
      12D01212D01212D01212D01212D01212D01212D01212D01212D01212D01212D0
      3838F00000A40000A42626DE0000BE0000BE0000BE0000BE0000BE0000BE0000
      BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE0000BE26
      26DE0000A40000A43838F01212D01212D01212D01212D01212D01212D01212D0
      1212D01212D01212D01212D01212D01212D01212D01212D01212D01212D03838
      F00000A400000000A42929EF0000CC0000CC0000CC9898EAFEFEFE0000CC0000
      CC0000CC0000CC0000CC0000CC0000CCFEFEFE9898EA0000CC0000CC0000CC29
      29EF0000A40000A42626DE0000BE0000BE0000BE3645C88698DC0000BE0000BE
      0000BE0000BE0000BE0000BE0000BE8AA0E53A4BD00000BE0000BE0000BE2626
      DE0000A40000A43838F01212D01212D01212D09F9FECFEFEFE1212D01212D012
      12D01212D01212D01212D01212D0FEFEFE9F9FEC1212D01212D01212D03838F0
      0000A40000A43838F01212D01212D01212D09F9FECFEFEFE1212D01212D01212
      D01212D01212D01212D01212D0FEFEFE9F9FEC1212D01212D01212D03838F000
      00A40000A42626DE0000BE0000BE0000BE6158BBBBAFCB0000BE0000BE0000BE
      0000BE0000BE0000BE0000BEBCB2CF635ABE0000BE0000BE0000BE2626DE0000
      A40000A43838F01212D01212D01212D09F9FECFEFEFE1212D01212D01212D012
      12D01212D01212D01212D0FEFEFE9F9FEC1212D01212D01212D03838F00000A4
      00000000A42929EF0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC
      0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC2929EF0000
      A40000A42626DE0000BE0000BE0000BE8697DC8697DC8698DD0000BE0000BE00
      00BE0000BE0000BE8A9FE48A9FE58AA0E50000BE0000BE0000BE2626DE0000A4
      0000A43838F01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212
      D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D03838F00000A400
      00A43838F01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D0
      1212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D03838F00000A40000
      A42626DE0000BE0000BE0000BEBAAECABAAECABAAECB0000BE0000BE0000BE00
      00BE0000BEBCB0CEBCB1CEBCB1CE0000BE0000BE0000BE2626DE0000A40000A4
      3838F01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D01212
      D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D03838F00000A400000000
      A42929EF0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC00
      00CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC2929EF0000A40000A4
      2626DE0000BE0000BE0000BE0000BE8697DC8698DC8699DE0000BE0000BE0000
      BE899EE28A9EE48A9FE40000BE0000BE0000BE0000BE2626DE0000A40000A438
      38F01212D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D0
      FEFEFEFEFEFEFEFEFE1212D01212D01212D01212D03838F00000A40000A43838
      F01212D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D0FE
      FEFEFEFEFEFEFEFE1212D01212D01212D01212D03838F00000A40000A42626DE
      0000BE0000BE0000BE0000BEBAAECABAAECABAAECB0000BE0000BE0000BEBBAF
      CDBBAFCEBBAFCE0000BE0000BE0000BE0000BE2626DE0000A40000A43838F012
      12D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D0FEFEFE
      FEFEFEFEFEFE1212D01212D01212D01212D03838F00000A400000000A42929EF
      0000CC0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CCFEFEFEFEFE
      FEEEEEFB0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42626DE00
      00BE0000BE0000BE0000BE0000BE8697DC8698DD8799DE0000BE889CE0889DE2
      7A8EDF0000BE0000BE0000BE0000BE0000BE2626DE0000A40000A43838F01212
      D01212D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D0FEFEFEFEFEFEEF
      EFFB1212D01212D01212D01212D01212D03838F00000A40000A43838F01212D0
      1212D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D0FEFEFEFEFEFEEFEF
      FB1212D01212D01212D01212D01212D03838F00000A40000A42626DE0000BE00
      00BE0000BE0000BE0000BEB9ACCAB9ACCABAADCB0000BEBAAECCBAAECDAB9FCA
      0000BE0000BE0000BE0000BE0000BE2626DE0000A40000A43838F01212D01212
      D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D0FEFEFEFEFEFEEFEFFB12
      12D01212D01212D01212D01212D03838F00000A400000000A42929EF0000CC00
      00CC0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE0000CC
      0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42626DE0000BE0000
      BE0000BE0000BE0000BE0000BE8697DC8699DE8799DE879ADF889CE10000BE00
      00BE0000BE0000BE0000BE0000BE2626DE0000A40000A43838F01212D01212D0
      1212D01212D01212D01212D0FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE1212D01212
      D01212D01212D01212D01212D03838F00000A40000A43838F01212D01212D012
      12D01212D01212D01212D0FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE1212D01212D0
      1212D01212D01212D01212D03838F00000A40000A42626DE0000BE0000BE0000
      BE0000BE0000BE0000BEB9ACCAB9ACCAB9ACCBBAADCBBAAECC0000BE0000BE00
      00BE0000BE0000BE0000BE2626DE0000A40000A43838F01212D01212D01212D0
      1212D01212D01212D0FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE1212D01212D01212
      D01212D01212D01212D03838F00000A400000000A42929EF0000CC0000CC0000
      CC0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC2929EF0000A40000A42626DE0000BE0000BE0000BE
      0000BE0000BE0000BE0000BE8698DD8699DE8799DE0000BE0000BE0000BE0000
      BE0000BE0000BE0000BE2626DE0000A40000A43838F01212D01212D01212D012
      12D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D01212D0
      1212D01212D01212D03838F00000A40000A43838F01212D01212D01212D01212
      D01212D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D01212D012
      12D01212D01212D03838F00000A40000A42626DE0000BE0000BE0000BE0000BE
      0000BE0000BE0000BEB9ABCAB9ACCAB9ACCB0000BE0000BE0000BE0000BE0000
      BE0000BE0000BE2626DE0000A40000A43838F01212D01212D01212D01212D012
      12D01212D01212D0FEFEFEFEFEFEFEFEFE1212D01212D01212D01212D01212D0
      1212D01212D03838F00000A400000000A42929EF0000CC0000CC0000CC0000CC
      0000CC0000CCFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000
      CC0000CC0000CC2929EF0000A40000A42626DE0000BE0000BE0000BE0000BE00
      00BE0000BE8596DB8697DC8698DD8698DD8799DE0000BE0000BE0000BE0000BE
      0000BE0000BE2626DE0000A40000A43838F01212D01212D01212D01212D01212
      D01212D0FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE1212D01212D01212D01212D012
      12D01212D03838F00000A40000A43838F01212D01212D01212D01212D01212D0
      1212D0FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE1212D01212D01212D01212D01212
      D01212D03838F00000A40000A42626DE0000BE0000BE0000BE0000BE0000BE00
      00BEB8AAC9B9ABC9B9ABCAB9ABCAB9ABCA0000BE0000BE0000BE0000BE0000BE
      0000BE2626DE0000A40000A43838F01212D01212D01212D01212D01212D01212
      D0FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE1212D01212D01212D01212D01212D012
      12D03838F00000A400000000A42929EF0000CC0000CC0000CC0000CC0000CCFE
      FEFEFEFEFEFEFEFE0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC
      0000CC2929EF0000A40000A42929EF0000CC0000CC0000CC0000CC0000CC8595
      DA8596DB8596DB0000CC8697DC8698DD8698DD0000CC0000CC0000CC0000CC00
      00CC2929EF0000A40000A42929EF0000CC0000CC0000CC0000CC0000CCFEFEFE
      FEFEFEFEFEFE0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC0000
      CC2929EF0000A40000A42929EF0000CC0000CC0000CC0000CC0000CCFEFEFEFE
      FEFEFEFEFE0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC0000CC
      2929EF0000A40000A42929EF0000CC0000CC0000CC0000CC0000CCB8AAC9B8AA
      CAB8AACA0000CCB8AACAB9ABCBB9AACB0000CC0000CC0000CC0000CC0000CC29
      29EF0000A40000A42929EF0000CC0000CC0000CC0000CC0000CCFEFEFEFEFEFE
      FEFEFE0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC0000CC2929
      EF0000A400000000A42929EF0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFE
      FE0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC29
      29EF0000A40000A42929EF0000CC0000CC0000CC0000CC8494D88595D98595DA
      0000CC0000CC0000CC8697DC8698DD8698DD0000CC0000CC0000CC0000CC2929
      EF0000A40000A42929EF0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE00
      00CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC2929EF
      0000A40000A42929EF0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000
      CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC2929EF00
      00A40000A42929EF0000CC0000CC0000CC0000CCB8AAC8B8AAC9B8AAC90000CC
      0000CC0000CCB8AACAB9AACBB9AACB0000CC0000CC0000CC0000CC2929EF0000
      A40000A42929EF0000CC0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC00
      00CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000CC2929EF0000A4
      00000000A42929EF0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC
      0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC2929EF0000
      A40000A42929EF0000CC0000CC0000CC8493D48494D68494D70000CC0000CC00
      00CC0000CC0000CC8697DC8697DC8697DC0000CC0000CC0000CC2929EF0000A4
      0000A42929EF0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000
      CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC2929EF0000A400
      00A42929EF0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC
      0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC2929EF0000A40000
      A42929EF0000CC0000CC0000CCB7A9C6B8AAC7B8AAC80000CC0000CC0000CC00
      00CC0000CCB8AACAB8AACAB8AACA0000CC0000CC0000CC2929EF0000A40000A4
      2929EF0000CC0000CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC0000
      CC0000CCFEFEFEFEFEFEFEFEFE0000CC0000CC0000CC2929EF0000A400000000
      A42929EF0000CC0000CC0000CC9898EAFEFEFE0000CC0000CC0000CC0000CC00
      00CC0000CC0000CCFEFEFE9898EA0000CC0000CC0000CC2929EF0000A40000A4
      2929EF0000CC0000CC0000CC3541C68493D40000CC0000CC0000CC0000CC0000
      CC0000CC0000CC8697DB3645CE0000CC0000CC0000CC2929EF0000A40000A429
      29EF0000CC0000CC0000CC9898EAFEFEFE0000CC0000CC0000CC0000CC0000CC
      0000CC0000CCFEFEFE9898EA0000CC0000CC0000CC2929EF0000A40000A42929
      EF0000CC0000CC0000CC9898EAFEFEFE0000CC0000CC0000CC0000CC0000CC00
      00CC0000CCFEFEFE9898EA0000CC0000CC0000CC2929EF0000A40000A42929EF
      0000CC0000CC0000CC5E53BBB7A9C60000CC0000CC0000CC0000CC0000CC0000
      CC0000CCB8AACA5F54BF0000CC0000CC0000CC2929EF0000A40000A42929EF00
      00CC0000CC0000CC9898EAFEFEFE0000CC0000CC0000CC0000CC0000CC0000CC
      0000CCFEFEFE9898EA0000CC0000CC0000CC2929EF0000A400000000A42929EF
      0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF00
      00CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC
      0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC00
      00CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC0000CC2929EF0000A400000000A42929EF0000CC00
      00CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000
      CC0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC00
      00CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000
      CC0000CC0000CC0000CC2929EF0000A400000000A42929EF0000CC0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000
      CC0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC
      0000CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC0000CC0000
      CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC0000CC0000CC
      0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000
      CC0000CC0000CC2929EF0000A40000A42929EF0000CC0000CC0000CC0000CC00
      00CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC0000CC
      0000CC0000CC2929EF0000A400000000A42929EF2929EF2929EF2929EF2929EF
      2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929
      EF2929EF2929EF2929EF0000A40000A42929EF2929EF2929EF2929EF2929EF29
      29EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF
      2929EF2929EF2929EF0000A40000A42929EF2929EF2929EF2929EF2929EF2929
      EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF29
      29EF2929EF2929EF0000A40000A42929EF2929EF2929EF2929EF2929EF2929EF
      2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929
      EF2929EF2929EF0000A40000A42929EF2929EF2929EF2929EF2929EF2929EF29
      29EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF
      2929EF2929EF0000A40000A42929EF2929EF2929EF2929EF2929EF2929EF2929
      EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF2929EF29
      29EF2929EF0000A400000000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A400
      00A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000A4
      0000A40000A40000A40000A40000A40000A40000A40000A40000A40000A40000
      A40000A40000}
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 0
    MarginBottom = 0
    Transparent = False
    TransparentColor = 50266367
    RowCount = 1
    ColCount = 6
    Left = 88
    Top = 24
  end
  object npdBottomBorder: TFsNinePitchDrawable
    Graphic.Data = {
      960D0000424D960D00000000000036000000280000008E000000080000000100
      180000000000600D0000000000000000000000000000000000008F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57300000CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA1780000CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA17800008F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57300000874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A200000C99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      0000C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A
      6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6E0000874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A200000}
    MarginLeft = 7
    MarginRight = 7
    MarginTop = 0
    MarginBottom = 0
    Transparent = False
    TransparentColor = 50266367
    RowCount = 2
    ColCount = 1
    Left = 32
    Top = 104
  end
  object npdLeftBorder: TFsNinePitchDrawable
    Graphic.Data = {
      96090000424D9609000000000000360000002800000008000000640000000100
      1800000000006009000000000000000000000000000000000000874A20874A20
      874A20874A208F57308F57308F57308F5730874A20C99A6EC99A6EC99A6E8F57
      30CDA178CDA178CDA178874A20C99A6EC99A6EC99A6E8F5730CDA178CDA178CD
      A178874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6EC99A6E8F5730CDA178CDA178CDA178874A20C99A6E9F62329F62328F57
      30CDA178A66D40A66D40874A20C99A6E9F62329F62328F5730CDA178A66D40A6
      6D40874A20C99A6E9F62329F62328F5730CDA178A66D40A66D40874A20C99A6E
      9F62329F62328F5730CDA178A66D40A66D40874A20C99A6E9F62329F62328F57
      30CDA178A66D40A66D40874A20C99A6E9F62329F62328F5730CDA178A66D40A6
      6D40874A20C99A6E9F62329F62328F5730CDA178A66D40A66D40874A20C99A6E
      9F62329F62328F5730CDA178A66D40A66D40874A20C99A6E9F62329F62328F57
      30CDA178A66D40A66D40874A20C99A6E9F62329F62328F5730CDA178A66D40A6
      6D40874A20C99A6E9F62329F62328F5730CDA178A66D40A66D40874A20C99A6E
      9F62329F62328F5730CDA178A66D40A66D40874A20C99A6E9F62329F62328F57
      30CDA178A66D40A66D40874A20CF9F72A46534A465348F5730D2A67CAA7042AA
      7042874A20CF9F72A46534A465348F5730D2A67CAA7042AA7042874A20CF9F72
      A46534A465348F5730D2A67CAA7042AA7042874A20CF9F72A46534A465348F57
      30D2A67CAA7042AA7042874A20CF9F72A46534A465348F5730D2A67CAA7042AA
      7042874A20CF9F72A46534A465348F5730D2A67CAA7042AA7042874A20CF9F72
      A46534A465348F5730D2A67CAA7042AA7042874A20CF9F72A46534A465348F57
      30D2A67CAA7042AA7042874A20CF9F72A46534A465348F5730D2A67CAA7042AA
      7042874A20CF9F72A46534A465348F5730D2A67CAA7042AA7042874A20CF9F72
      A46534A465348F5730D2A67CAA7042AA7042874A20CF9F72A46534A465348F57
      30D2A67CAA7042AA7042874A20CF9F72CF9F72CF9F728F5730D2A67CD2A67CD2
      A67C874A20874A20874A20874A208F57308F57308F57308F5730}
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 29
    MarginBottom = 4
    Transparent = False
    TransparentColor = 50266367
    RowCount = 1
    ColCount = 2
    Left = 104
    Top = 104
  end
  object npdMaxButton: TFsNinePitchDrawable
    Graphic.Data = {
      621F0000424D621F00000000000036000000280000007E000000150000000100
      1800000000002C1F000000000000000000000000000000000000874A20874A20
      874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A
      20874A20874A20874A20874A20874A20874A20874A20874A207D451E7D451E7D
      451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E
      7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E8F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57307D451E7D451E7D451E7D
      451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E
      7D451E7D451E7D451E7D451E7D451E7D451E7D451E8F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57300000874A20C99A6EC99A6EC9
      9A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6EC99A6E
      C99A6EC99A6EC99A6EC99A6EC99A6EC99A6E874A207D451EBB8F66BB8F66BB8F
      66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB
      8F66BB8F66BB8F66BB8F66BB8F66BB8F667D451E8F5730CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA178CDA1788F57308F5730CDA178CDA178CDA178CD
      A178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA1788F57307D451EBB8F66BB8F66BB8F66BB8F
      66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB8F66BB
      8F66BB8F66BB8F66BB8F66BB8F667D451E8F5730CDA178CDA178CDA178CDA178
      CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA178CDA1
      78CDA178CDA178CDA178CDA1788F57300000874A20C99A6E9F62329F62329F62
      329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F
      62329F62329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E
      945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F62329F62329F6232
      9F62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F62
      329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40CDA1788F57300000874A20C99A6E9F62329F62329F62329F62329F62329F
      62329F62329F62329F62329F62329F62329F62329F62329F62329F62329F6232
      9F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E94
      5B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40CDA1788F57308F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      CDA1788F57307D451EBB8F66945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2EBB
      8F667D451E8F5730CDA178A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40CDA1
      788F57300000874A20C99A6E9F62329F62329F6232FEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE9F62329F62329F6232C9
      9A6E874A207D451EBB8F66945B2E945B2E945B2ED7A177D7A177D7A177D7A177
      D7A177D7A177D7A177D7A177D7A177D7A177D7A177945B2E945B2E945B2EBB8F
      667D451E8F5730CDA178A66D40A66D40A66D40FEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEA66D40A66D40A66D40CDA178
      8F57308F5730CDA178A66D40A66D40A66D40FEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEA66D40A66D40A66D40CDA1788F
      57307D451EBB8F66945B2E945B2E945B2ED7A177D7A177D7A177D7A177D7A177
      D7A177D7A177D7A177D7A177D7A177D7A177945B2E945B2E945B2EBB8F667D45
      1E8F5730CDA178A66D40A66D40A66D40FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEA66D40A66D40A66D40CDA1788F5730
      0000874A20C99A6E9F62329F62329F6232FEFEFE9F62329F62329F62329F6232
      9F62329F62329F62329F62329F6232FEFEFE9F62329F62329F6232C99A6E874A
      207D451EBB8F66945B2E945B2E945B2ED7A177945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2E945B2ED7A177945B2E945B2E945B2EBB8F667D451E
      8F5730CDA178A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57308F
      5730CDA178A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57307D45
      1EBB8F66945B2E945B2E945B2ED7A177945B2E945B2E945B2E945B2E945B2E94
      5B2E945B2E945B2E945B2ED7A177945B2E945B2E945B2EBB8F667D451E8F5730
      CDA178A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D
      40A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57300000874A
      20C99A6E9F62329F62329F6232FEFEFE9F62329F62329F62329F62329F62329F
      62329F62329F62329F6232FEFEFE9F62329F62329F6232C99A6E874A207D451E
      BB8F66945B2E945B2E945B2ED7A177945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2E945B2ED7A177945B2E945B2E945B2EBB8F667D451E8F5730CD
      A178A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57308F5730CDA1
      78A66D40A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57307D451EBB8F66
      945B2E945B2E945B2ED7A177945B2E945B2E945B2E945B2E945B2E945B2E945B
      2E945B2E945B2ED7A177945B2E945B2E945B2EBB8F667D451E8F5730CDA178A6
      6D40A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40
      A66D40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57300000874A20C99A6E
      9F62329F62329F6232FEFEFE9F62329F62329F62329F62329F62329F62329F62
      329F62329F6232FEFEFE9F62329F62329F6232C99A6E874A207D451EBB8F6694
      5B2E945B2E945B2ED7A177945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2E945B2ED7A177945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D
      40A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40
      A66D40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40A66D40FEFEFEA66D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E94
      5B2E945B2ED7A177945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E
      945B2ED7A177945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D
      40A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A6
      6D40FEFEFEA66D40A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F
      62329F6232FEFEFE9F62329F62329F62329F62329F62329F62329F62329F6232
      9F6232FEFEFE9F62329F62329F6232C99A6E874A207D451EBB8F66945B2E945B
      2E945B2ED7A177945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E94
      5B2ED7A177945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40
      A66D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D
      40FEFEFEA66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A6
      6D40FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40
      FEFEFEA66D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B
      2ED7A177945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2ED7
      A177945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40
      FEFEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40FEFE
      FEA66D40A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F62329F62
      32FEFEFE9F62329F62329F62329F62329F62329F62329F62329F62329F6232FE
      FEFE9F62329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2E
      D7A177945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2ED7A1
      77945B2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40FE
      FEFEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40FEFEFE
      A66D40A66D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40FEFE
      FEA66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40FEFEFEA6
      6D40A66D40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2ED7A177
      945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2ED7A177945B
      2E945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40FEFEFEA6
      6D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40FEFEFEA66D40
      A66D40A66D40CDA1788F57300000874A20C99A6E9F62329F62329F6232FEFEFE
      9F62329F62329F62329F62329F62329F62329F62329F62329F6232FEFEFE9F62
      329F62329F6232C99A6E874A207D451EBB8F66945B2E945B2E945B2ED7A17794
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2ED7A177945B2E
      945B2E945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40FEFEFEA66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40FEFEFEA66D40A6
      6D40A66D40CDA1788F57308F5730CDA178A66D40A66D40A66D40FEFEFEA66D40
      A66D40A66D40A66D40A66D40A66D40A66D40A66D40A66D40FEFEFEA66D40A66D
      40A66D40CDA1788F57307D451EBB8F66945B2E945B2E945B2ED7A177945B2E94
      5B2E945B2E945B2E945B2E945B2E945B2E945B2E945B2ED7A177945B2E945B2E
      945B2EBB8F667D451E8F5730CDA178A66D40A66D40A66D40FEFEFEA66D40A66D
      40A66D40A66D40A66D40A66D40A66D40A66D40A66D40FEFEFEA66D40A66D40A6
      6D40CDA1788F57300000874A20CF9F72A46534A46534A46534FEFEFEA46534A4
      6534A46534A46534A46534A46534A46534A46534A46534FEFEFEA46534A46534
      A46534CF9F72874A207D451EC0946A985E30985E30985E30D7A177985E30985E
      30985E30985E30985E30985E30985E30985E30985E30D7A177985E30985E3098
      5E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042FEFEFEAA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042FEFEFEAA7042AA7042AA70
      42D2A67C8F57308F5730D2A67CAA7042AA7042AA7042FEFEFEAA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042FEFEFEAA7042AA7042AA7042
      D2A67C8F57307D451EC0946A985E30985E30985E30D7A177985E30985E30985E
      30985E30985E30985E30985E30985E30985E30D7A177985E30985E30985E30C0
      946A7D451E8F5730D2A67CAA7042AA7042AA7042FEFEFEAA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042FEFEFEAA7042AA7042AA7042D2A6
      7C8F57300000874A20CF9F72A46534A46534A46534FEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEA46534A46534A46534CF
      9F72874A207D451EC0946A985E30985E30985E30D7A177D7A177D7A177D7A177
      D7A177D7A177D7A177D7A177D7A177D7A177D7A177985E30985E30985E30C094
      6A7D451E8F5730D2A67CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C
      8F57308F5730D2A67CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F
      57307D451EC0946A985E30985E30985E30D7A177D7A177D7A177D7A177D7A177
      D7A177D7A177D7A177D7A177D7A177D7A177985E30985E30985E30C0946A7D45
      1E8F5730D2A67CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F5730
      0000874A20CF9F72A46534A46534A46534FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEA46534A46534A46534CF9F72874A
      207D451EC0946A985E30985E30985E30D7A177D7A177D7A177D7A177D7A177D7
      A177D7A177D7A177D7A177D7A177D7A177985E30985E30985E30C0946A7D451E
      8F5730D2A67CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F57308F
      5730D2A67CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F57307D45
      1EC0946A985E30985E30985E30D7A177D7A177D7A177D7A177D7A177D7A177D7
      A177D7A177D7A177D7A177D7A177985E30985E30985E30C0946A7D451E8F5730
      D2A67CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F57300000874A
      20CF9F72A46534A46534A46534FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEFEFEA46534A46534A46534CF9F72874A207D451E
      C0946A985E30985E30985E30D7A177D7A177D7A177D7A177D7A177D7A177D7A1
      77D7A177D7A177D7A177D7A177985E30985E30985E30C0946A7D451E8F5730D2
      A67CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F57308F5730D2A6
      7CAA7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F57307D451EC0946A
      985E30985E30985E30D7A177D7A177D7A177D7A177D7A177D7A177D7A177D7A1
      77D7A177D7A177D7A177985E30985E30985E30C0946A7D451E8F5730D2A67CAA
      7042AA7042AA7042FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
      FEFEFEFEFEFEFEFEFEAA7042AA7042AA7042D2A67C8F57300000874A20CF9F72
      A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A465
      34A46534A46534A46534A46534A46534A46534CF9F72874A207D451EC0946A98
      5E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30
      985E30985E30985E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042D2A67C8F57308F5730D2A67CAA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042D2A67C8F57307D451EC0946A985E3098
      5E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30
      985E30985E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042D2A67C8F57300000874A20CF9F72A46534A4
      6534A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534
      A46534A46534A46534A46534A46534CF9F72874A207D451EC0946A985E30985E
      30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E3098
      5E30985E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042D2A67C8F57308F5730D2A67CAA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042AA7042D2A67C8F57307D451EC0946A985E30985E30985E
      30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E3098
      5E30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042
      AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA70
      42AA7042AA7042AA7042D2A67C8F57300000874A20CF9F72A46534A46534A465
      34A46534A46534A46534A46534A46534A46534A46534A46534A46534A46534A4
      6534A46534A46534A46534CF9F72874A207D451EC0946A985E30985E30985E30
      985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E
      30985E30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042AA7042D2A67C8F57308F5730D2A67CAA7042AA7042AA7042AA70
      42AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA
      7042AA7042AA7042D2A67C8F57307D451EC0946A985E30985E30985E30985E30
      985E30985E30985E30985E30985E30985E30985E30985E30985E30985E30985E
      30985E30985E30C0946A7D451E8F5730D2A67CAA7042AA7042AA7042AA7042AA
      7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042AA7042
      AA7042AA7042D2A67C8F57300000874A20CF9F72CF9F72CF9F72CF9F72CF9F72
      CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F72CF9F
      72CF9F72CF9F72CF9F72874A207D451EC0946AC0946AC0946AC0946AC0946AC0
      946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946A
      C0946AC0946AC0946A7D451E8F5730D2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67CD2A67C8F57308F5730D2A67CD2A67CD2A67CD2A67CD2A67CD2A67C
      D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67C8F57307D451EC0946AC0946AC0946AC0946AC0946AC0946AC0
      946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946AC0946A
      C0946AC0946A7D451E8F5730D2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A6
      7CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2A67CD2
      A67CD2A67C8F57300000874A20874A20874A20874A20874A20874A20874A2087
      4A20874A20874A20874A20874A20874A20874A20874A20874A20874A20874A20
      874A20874A20874A207D451E7D451E7D451E7D451E7D451E7D451E7D451E7D45
      1E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D
      451E7D451E7D451E8F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F
      57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57307D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D45
      1E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D451E7D
      451E7D451E8F57308F57308F57308F57308F57308F57308F57308F57308F5730
      8F57308F57308F57308F57308F57308F57308F57308F57308F57308F57308F57
      308F57300000}
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 0
    MarginBottom = 0
    Transparent = False
    TransparentColor = 50266367
    RowCount = 1
    ColCount = 6
    Left = 120
    Top = 24
  end
  object npdRightBorder: TFsNinePitchDrawable
    Graphic.Data = {
      96090000424D9609000000000000360000002800000008000000640000000100
      1800000000006009000000000000000000000000000000000000874A20874A20
      874A20874A208F57308F57308F57308F5730C99A6EC99A6EC99A6E874A20CDA1
      78CDA178CDA1788F5730C99A6EC99A6EC99A6E874A20CDA178CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730874A20C99A6E
      C99A6E874A208F5730CDA178CDA1788F5730874A20C99A6EC99A6E874A208F57
      30CDA178CDA1788F5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F
      5730874A20C99A6EC99A6E874A208F5730CDA178CDA1788F5730C99A6EC99A6E
      C99A6E874A20CDA178CDA178CDA1788F57309F62329F6232C99A6E874A20A66D
      40A66D40CDA1788F57309F62329F6232C99A6E874A20A66D40A66D40CDA1788F
      57309F62329F6232C99A6E874A20A66D40A66D40CDA1788F57309F62329F6232
      C99A6E874A20A66D40A66D40CDA1788F57309F62329F6232C99A6E874A20A66D
      40A66D40CDA1788F57309F62329F6232C99A6E874A20A66D40A66D40CDA1788F
      57309F62329F6232C99A6E874A20A66D40A66D40CDA1788F57309F62329F6232
      C99A6E874A20A66D40A66D40CDA1788F57309F62329F6232C99A6E874A20A66D
      40A66D40CDA1788F57309F62329F6232C99A6E874A20A66D40A66D40CDA1788F
      57309F62329F6232C99A6E874A20A66D40A66D40CDA1788F57309F62329F6232
      C99A6E874A20A66D40A66D40CDA1788F57309F62329F6232C99A6E874A20A66D
      40A66D40CDA1788F5730A46534A46534CF9F72874A20AA7042AA7042D2A67C8F
      5730A46534A46534CF9F72874A20AA7042AA7042D2A67C8F5730A46534A46534
      CF9F72874A20AA7042AA7042D2A67C8F5730A46534A46534CF9F72874A20AA70
      42AA7042D2A67C8F5730A46534A46534CF9F72874A20AA7042AA7042D2A67C8F
      5730A46534A46534CF9F72874A20AA7042AA7042D2A67C8F5730A46534A46534
      CF9F72874A20AA7042AA7042D2A67C8F5730A46534A46534CF9F72874A20AA70
      42AA7042D2A67C8F5730A46534A46534CF9F72874A20AA7042AA7042D2A67C8F
      5730A46534A46534CF9F72874A20AA7042AA7042D2A67C8F5730A46534A46534
      CF9F72874A20AA7042AA7042D2A67C8F5730A46534A46534CF9F72874A20AA70
      42AA7042D2A67C8F5730CF9F72CF9F72CF9F72874A20D2A67CD2A67CD2A67C8F
      5730874A20874A20874A20874A208F57308F57308F57308F5730}
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 29
    MarginBottom = 4
    Transparent = False
    TransparentColor = 50266367
    RowCount = 1
    ColCount = 2
    Left = 168
    Top = 88
  end
  object FsFlatScrollBar1: TFsFlatScrollBar
    HScrollHeight = 17
    VScrollWidth = 17
    VArrowHeight = 17
    HArrowWidth = 17
    MinThumbLength = 20
    Left = 64
    Top = 176
  end
end
