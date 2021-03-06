unit FSStdCtrls;

interface

uses
  SysUtils, Classes, Consts, Windows, Graphics, Controls, Messages, StdCtrls, ExtCtrls, ComCtrls,
  Forms, Themes, Menus, FSVclBase, FSGraphics, FSControls, FSScrollControls;

type
  TFsImage = class(TFsGraphicControl)
  private
    FPicture: TFsDrawable;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TFsDrawable);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetContentDimension(out dim: TSize); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Picture: TFsDrawable read FPicture write SetPicture;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property Enabled;
    property ShowHint;
    property Visible;
    property OnClick;
  end;

  TFsImageButtonLayout = (blPictureLeft, blPictureRight, blPictureTop, blPictureBottom, blTextInPicture, blTextInStretchPicture);

  TFsCustomButton = class(TFsGraphicControl)
  private
    FShowCaption: Boolean;
    FLayout: TFsImageButtonLayout;
    FDown: Boolean;
    FGroup: Integer;
    FAllowDown: Boolean;
    FSpace: Integer;
    procedure SetShowCaption(const Value: Boolean);
    procedure SetLayout(const Value: TFsImageButtonLayout);
    procedure DrawPictureLeft;
    procedure DrawPictureRight;
    procedure DrawPictureTop;
    procedure DrawPictureBottom;
    procedure DrawTextInPicture;
    procedure DrawTextInStretchPicture;
    procedure SetDown(const Value: Boolean);
    procedure SetGroup(const Value: Integer);
    procedure SetAllowDown(const Value: Boolean);
    procedure SetSpace(const Value: Integer);
  protected
    FMouseFlag: TMouseFlag;
    procedure SetOtherUp;
    procedure DrawImageAndText;
    procedure GetContentDimension(out dim: TSize); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChange(var msg: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var msgr: TMessage); message CM_FONTCHANGED;
    function PictureIsHorzStretchable: Boolean; virtual;
    function PictureIsVertStretchable: Boolean; virtual;
    function GetPictureSize(out width: Integer): Integer; virtual; abstract;
    procedure DrawPicture(const Rect: TRect); virtual; abstract;
    procedure Paint; override;
    property Down: Boolean read FDown write SetDown;
    property AllowDown: Boolean read FAllowDown write SetAllowDown;
    property Group: Integer read FGroup write SetGroup;
  public
    constructor Create(Owner: TComponent); override;
  published
    property Layout: TFsImageButtonLayout read FLayout write SetLayout default blTextInStretchPicture;
    property Space: Integer read FSpace write SetSpace default 4;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property Caption;
    property Enabled;
    property Font;
    property ShowHint;
    property Visible;
    property OnClick;
  end;

  TFsImageButton = class(TFsCustomButton)
  private
    FPicture: TFsPicture;
    FDisablePicture: TFsPicture;
    FMouseOverPicture: TFsPicture;
    FMouseDownPicture: TFsPicture;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TFsPicture);
    procedure SetMouseDownPicture(const Value: TFsPicture);
    procedure SetMouseOverPicture(const Value: TFsPicture);
    procedure SetDisablePicture(const Value: TFsPicture);
    function GetDrawable: TFsPicture;
  protected
    function GetPictureSize(out width: Integer): Integer; override;
    procedure DrawPicture(const Rect: TRect); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property AllowDown;
    property Background;
    property Down;
    property Group;
    property Picture: TFsPicture read FPicture write SetPicture;
    property DisablePicture: TFsPicture read FDisablePicture write SetDisablePicture;
    property MouseOverPicture: TFsPicture read FMouseOverPicture write SetMouseOverPicture;
    property MouseDownPicture: TFsPicture read FMouseDownPicture write SetMouseDownPicture;
  end;

  TFsCoverButton = class(TFsCustomButton)
  private
    FPicture: TFsPicture;
    FHoverCover: TFsDrawable;
    FDownCover: TFsDrawable;
    procedure SetPicture(const Value: TFsPicture);
    procedure SetDownCover(const Value: TFsDrawable);
    procedure SetHoverCover(const Value: TFsDrawable);
    function GetCover: TFsDrawable;
  protected
    procedure PictureChanged(Sender: TObject);
    procedure LinkedPictureChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetPictureSize(out width: Integer): Integer; override;
    procedure DrawPicture(const Rect: TRect); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AllowDown;
    property Background;
    property Down;
    property Group;
    property Picture: TFsPicture read FPicture write SetPicture;
    property HoverCover: TFsDrawable read FHoverCover write SetHoverCover;
    property DownCover: TFsDrawable read FDownCover write SetDownCover;
  end;

  TFsMemo = class(TFsScrollContainer)
  private
    function GetScrollBars: TScrollStyle;
    procedure SetScrollBars(const Value: TScrollStyle);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    function GetMemo: TCustomMemo;
  protected
    function CreateRealControl: TControl; override;
  public
    property Memo: TCustomMemo read GetMemo;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property Ctl3D;
    property Visible;
    property ParentBackground;
    property ParentCtl3D;
    property ScrollBarDrawer;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars;
    property Lines: TStrings read GetLines write SetLines;
  end;

  TFsListBox = class(TFsScrollContainer)
  private
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    function GetListBox: TCustomListBox;
  protected
    function CreateRealControl: TControl; override;
  public
    property ListBox: TCustomListBox read GetListBox;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property Ctl3D;
    property Visible;
    property ParentBackground;
    property ParentCtl3D;
    property ScrollBarDrawer;
    property Items: TStrings read GetItems write SetItems;
  end;

  TFsListView = class(TFsHackThumbDragScrollContainer)
  private
    function GetItems: TListItems;
    procedure SetItems(const Value: TListItems);
    function GetColumns: TListColumns;
    procedure SetColumns(const Value: TListColumns);
    function GetViewStyle: TViewStyle;
    procedure SetViewStyle(const Value: TViewStyle);
    function GetListView: TCustomListView;
  protected
    function CreateRealControl: TControl; override;
  public
    property ListView: TCustomListView read GetListView;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property Ctl3D;
    property Visible;
    property ParentBackground;
    property ParentCtl3D;
    property ScrollBarDrawer;
    property Columns: TListColumns read GetColumns write SetColumns;
    property Items: TListItems read GetItems write SetItems;
    property ViewStyle: TViewStyle read GetViewStyle write SetViewStyle default vsIcon;
  end;

  TFsTreeView = class(TFsHackThumbDragScrollContainer)
  private
    function GetTreeView: TCustomTreeView;
    function GetItems: TTreeNodes;
    procedure SetItems(const Value: TTreeNodes);
  protected
    function CreateRealControl: TControl; override;
  public
    property TreeView: TCustomTreeView read GetTreeView;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property Ctl3D;
    property Visible;
    property ParentBackground;
    property ParentCtl3D;
    property ScrollBarDrawer;
    property Items: TTreeNodes read GetItems write SetItems;
  end;

  TFsPanel = class(TCustomPanel)
  protected
    procedure WMEraseBkgnd(var msgr: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TFsPageControl = class(TPageControl)
  protected
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
  public
  end;

  TFsCustomCombobox = class(TCustomComboBox)
  private
    FBorderColor: TColor;
    FBorderColorHover: TColor;
    FMouseInControlLastPaint: Boolean;
    FIsEditTrackingMouse: Boolean;
    FCanvas: TCanvas;
    FTip: string;
    FTipFont: TFont;
    FTextFont: TFont;
    FShowingTip: Boolean;
    FShowTip: Boolean;
    FNumberOnly: Boolean;
    FButtonWidth: Integer;
    procedure TipFontChanged(Sender: TObject);
    procedure TextFontChanged(Sender: TObject);
    procedure AdjustEditBoundsRect;
    procedure CalcEditBoundsRect(var r: TRect);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorHover(const Value: TColor);
    function MouseInControl: Boolean;
    procedure Paint;
    procedure CheckInvalidate;
    procedure SetNumberOnly(const Value: Boolean);
    procedure SetShowingTip(const Value: Boolean);
    procedure SetShowTip(const Value: Boolean);
    procedure SetTextFont(const Value: TFont);
    procedure SetTip(const Value: string);
    procedure SetTipFont(const Value: TFont);
    procedure SetButtonWidth(const Value: Integer);
    function GetText: string;
    procedure SetText(const Value: string);
    function ShouldStoreText: Boolean;
  protected
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DrawButton(const Rect: TRect); virtual; abstract;
    procedure CMMouseEnter(var msgr: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msgr: TMessage); message CM_MOUSELEAVE;
    procedure CBShowShopDown(var msgr: TMessage); message CB_SHOWDROPDOWN;
    procedure WMSize(var msgr: TWMSize); message WM_SIZE;
    procedure WMPaint(var msgr: TWMPaint); message WM_PAINT;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMLButtonDown(var msgr: TWMNCLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var msgr: TWMNCLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var msgr: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure EditWndProc(var msgr: TMessage); override;
    property NumberOnly: Boolean read FNumberOnly write SetNumberOnly;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 24;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderColorHover: TColor read FBorderColorHover write SetBorderColorHover;
    property ShowTip: Boolean read FShowTip write SetShowTip;
    property Tip: string read FTip write SetTip;
    property TipFont: TFont read FTipFont write SetTipFont;
    property Text: string read GetText write SetText stored ShouldStoreText;
    property TextFont: TFont read FTextFont write SetTextFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShowingTip: Boolean read FShowingTip write SetShowingTip;
  end;

  TFsCombobox = class(TFsCustomCombobox)
  private
    FButtonPicture: TFsPicture;
    procedure ButtonPictureChanged(Sender: TObject);
    procedure SetButtonPicture(const Value: TFsPicture);
  protected
    procedure DrawButton(const Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property AutoComplete default True;
    property AutoCompleteDelay default 500;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
    property ButtonWidth;
    property BorderColor;
    property BorderColorHover;
    property ShowTip;
    property Tip;
    property TipFont;
    property TextFont;
    property ButtonPicture: TFsPicture read FButtonPicture write SetButtonPicture;
  end;

  TFsCustomCheckBox = class(TFsGraphicControl)
  private
    FChecked: Boolean;
    FSpace: Integer;
    procedure SetChecked(const Value: Boolean);
    procedure SetSpace(const Value: Integer);
  protected
    procedure CMTextChange(var msg: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChange(var msg: TMessage); message CM_FONTCHANGED;
    procedure GetImageSize(out w, h: Integer); virtual;
    procedure DrawMark(const Rect: TRect); virtual; abstract;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure GetContentDimension(out dim: TSize); override;
  public
    constructor Create(Owner: TComponent); override;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property Enabled;
    property Font;
    property Hint;
    property ShowHint;
    property Visible;
    property OnClick;
    property Caption;
    property Checked: Boolean read FChecked write SetChecked;
    property Space: Integer read FSpace write SetSpace;
  end;

  TFsCheckBox = class(TFsCustomCheckBox)
  private
    FCheckedPicture: TFsPicture;
    FUnCheckedPicture: TFsPicture;
    procedure SetCheckedPicture(const Value: TFsPicture);
    procedure SetUnCheckedPicture(const Value: TFsPicture);
    procedure PictureChanged(Sender: TObject);
  protected
    procedure GetImageSize(out w, h: Integer); override;
    procedure DrawMark(const Rect: TRect); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property CheckedPicture: TFsPicture read FCheckedPicture write SetCheckedPicture;
    property UnCheckedPicture: TFsPicture read FUnCheckedPicture write SetUnCheckedPicture;
  end;

implementation

uses
  FSEmptyForm;

type
  TFsBorderlessMemo = class(TCustomMemo)
  protected
    procedure InheritedWndProc(var msgr: TMessage);
    procedure WndProc(var msgr: TMessage); override;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAINT;
  end;

  TFsBorderlessListBox = class(TCustomListBox)
  protected
    procedure InheritedWndProc(var msgr: TMessage);
    procedure WndProc(var msgr: TMessage); override;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAINT;
  end;

  TFsBorderlessListView = class(TCustomListView)
  protected
    procedure InheritedWndProc(var msgr: TMessage);
    procedure WndProc(var msgr: TMessage); override;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAINT;
  end;

  TFsBorderlessTreeView = class(TCustomTreeView)
  protected
    procedure InheritedWndProc(var msgr: TMessage);
    procedure WndProc(var msgr: TMessage); override;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAINT;
  end;

{ TFsCustomButton }

procedure TFsCustomButton.CMFontChanged(var msgr: TMessage);
begin
  Self.AutoSizeAndInvalidate;
end;

procedure TFsCustomButton.CMMouseEnter(var msg: TMessage);
begin
  inherited;

  Include(FMouseFlag, mfMouseOver);

  Self.AutoSizeAndInvalidate;
end;

procedure TFsCustomButton.CMMouseLeave(var msg: TMessage);
begin
  inherited;

  Exclude(FMouseFlag, mfMouseOver);

  Self.AutoSizeAndInvalidate;
end;

procedure TFsCustomButton.CMTextChange(var msg: TMessage);
begin
  Self.AutoSizeAndInvalidate;
end;

constructor TFsCustomButton.Create(Owner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable, csPannable];
  FSpace := 4;
  FShowCaption := True;
  FLayout := blTextInStretchPicture;
end;

procedure TFsCustomButton.DrawImageAndText;
var
  w: Integer;
begin
  Self.GetPictureSize(w);
  
  if w <> 0 then
  begin
    case Self.FLayout of
      blPictureLeft: Self.DrawPictureLeft;
      blPictureRight: Self.DrawPictureRight;
      blPictureTop: Self.DrawPictureTop;
      blPictureBottom: Self.DrawPictureBottom;
      blTextInPicture: Self.DrawTextInPicture;
      blTextInStretchPicture: Self.DrawTextInStretchPicture;
    end;
  end;
end;

procedure TFsCustomButton.DrawPictureBottom;
var
  Text: string;
  rtext, rpic: TRect;
  dim: TSize;
  w, h: Integer;
begin
  Self.GetContentDimension(dim);

  h := Self.GetPictureSize(w);

  if w > 0 then
  begin
    rpic.Bottom := Self.Height - (Self.Height - dim.cy) div 2;
    rpic.Top := rpic.Bottom - h;

    if PictureIsHorzStretchable and (dim.cx > w) then
    begin
      rpic.Left := (Self.Width - dim.cx) div 2;
      rpic.Right := rpic.Left + dim.cx;
    end
    else begin
      rpic.Left := (Self.Width - w) div 2;
      rpic.Right := rpic.Left + w;
    end;

    Self.DrawPicture(rpic);

    rpic.Top := rpic.top - FSpace;
  end
  else rpic.Top := Self.Height - (Self.Height - dim.cy) div 2;

  if FShowCaption and (Self.Caption <> '') then
  begin
    Text := Self.Caption;

    rtext.Top := (Self.Height - dim.cy) div 2;
    rtext.Bottom := rpic.Top;

    rtext.Left := (Self.Width - dim.cx) div 2;
    rtext.Right := Self.Width - rtext.Left;
    
    Canvas.Font := Self.Font;
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tfSingleLine]);
  end;
end;

procedure TFsCustomButton.DrawPictureLeft;
var
  Text: string;
  rtext, rpic: TRect;
  dim: TSize;
  w, h: Integer;
begin
  Self.GetContentDimension(dim);

  h := Self.GetPictureSize(w);

  if w > 0 then
  begin
    rpic.Left := (Self.Width - dim.cx) div 2;

    if PictureIsVertStretchable and (dim.cy > h) then
    begin
      rpic.Top := (Self.Height - dim.cy) div 2;
      rpic.Bottom := rpic.Top + dim.cy;
    end
    else begin
      rpic.Top := (Self.Height - h) div 2;
      rpic.Bottom := rpic.Top + h;
    end;

    rpic.Right := rpic.Left + w;

    Self.DrawPicture(rpic);

    rpic.Right := rpic.Right + FSpace;
  end
  else rpic.Right := (Self.Width - dim.cx) div 2;

  if FShowCaption and (Self.Caption <> '') then
  begin
    Text := Self.Caption;

    rtext.Left := rpic.Right;
    rtext.Right := Self.Width - (Self.Width - dim.cx) div 2;
    rtext.Top := (Self.Height - dim.cy) div 2;
    rtext.Bottom := Self.Height - (Self.Height - dim.cy) div 2;
    Canvas.Font := Self.Font;
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tfSingleLine]);
  end;
end;

procedure TFsCustomButton.DrawPictureRight;
var
  Text: string;
  rtext, rpic: TRect;
  w, h: Integer;
  dim: TSize;
begin
  Self.GetContentDimension(dim);

  h := Self.GetPictureSize(w);
  
  if w > 0 then
  begin
    rpic.Right := Self.Width - (Self.Width - dim.cx) div 2;

    if PictureIsVertStretchable and (dim.cy > h) then
    begin
      rpic.Top := (Self.Height - dim.cy) div 2;
      rpic.Bottom := rpic.Top + dim.cy;
    end
    else begin
      rpic.Top := (Self.Height - h) div 2;
      rpic.Bottom := rpic.Top + h;
    end;

    rpic.Left := rpic.Right - w;

    Self.DrawPicture(rpic);

    rpic.Left := rpic.Left - FSpace;
  end
  else rpic.Left := Self.Width - (Self.Width - dim.cx) div 2;

  if FShowCaption and (Self.Caption <> '') then
  begin
    Text := Self.Caption;

    rtext.Left := (Self.Width - dim.cx) div 2;
    rtext.Right := rpic.Left;
    rtext.Top := (Self.Height - dim.cy) div 2;
    rtext.Bottom := Self.Height - (Self.Height - dim.cy) div 2;
    Canvas.Font := Self.Font;
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tfSingleLine]);
  end;
end;

procedure TFsCustomButton.DrawPictureTop;
var
  Text: string;
  rtext, rpic: TRect;
  w, h: Integer;
  dim: TSize;
begin
  Self.GetContentDimension(dim);

  h := Self.GetPictureSize(w);

  if w > 0 then
  begin
    rpic.Top := (Self.Height - dim.cy) div 2;
    rpic.Bottom := rpic.Top + h;

    if PictureIsHorzStretchable and (dim.cx > w) then
    begin
      rpic.Left := (Self.Width - dim.cx) div 2;
      rpic.Right := rpic.Left + dim.cx;
    end
    else begin
      rpic.Left := (Self.Width - w) div 2;
      rpic.Right := rpic.Left + w;
    end;

    Self.DrawPicture(rpic);

    rpic.Bottom := rpic.Bottom + FSpace;
  end
  else rpic.Bottom := (Self.Height - dim.cy) div 2;

  if FShowCaption and (Self.Caption <> '') then
  begin
    Text := Self.Caption;

    rtext.Top := rpic.Bottom;
    rtext.Bottom := Self.Height - (Self.Height - dim.cy) div 2;

    rtext.Left := (Self.Width - dim.cx) div 2;
    rtext.Right := Self.Width - rtext.Left;
    
    Canvas.Font := Self.Font;
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(rtext, Text, [tfCenter, tfVerticalCenter, tfSingleLine]);
  end;
end;

procedure TFsCustomButton.DrawTextInPicture;
var
  Text: string;
  r: TRect;
  w, h: Integer;
  dim: TSize;
begin
  Self.GetContentDimension(dim);

  h := Self.GetPictureSize(w);

  if w > 0 then
  begin
    if PictureIsHorzStretchable and (w < dim.cx) then
      r.Left := (Self.Width - dim.cx) div 2
    else
      r.Left := (Self.Width - w) div 2;

    r.Right := Self.Width - r.Left;

    if PictureIsVertStretchable and (h < dim.cy) then
      r.Top := (Self.Height - dim.cy) div 2
    else
      r.Top := (Self.Height - h) div 2;

    r.Bottom := Self.Height - r.Top;
    
    Self.DrawPicture(r);
  end;

  if FShowCaption then
  begin
    Text := Caption;

    if Text <> '' then
    begin
      r.Left := 0;
      r.Right := Self.Width;
      r.Top := 0;
      r.Bottom := Self.Height;

      Canvas.Font := Self.Font;
      Canvas.Brush.Style := bsClear;
      Canvas.TextRect(r, Text, [tfCenter, tfVerticalCenter, tfSingleLine]);
    end;
  end;
end;

procedure TFsCustomButton.DrawTextInStretchPicture;
var
  Text: string;
  r: TRect;
  w: Integer;
begin
  r.Left := 0;
  r.Right := Self.Width;
  r.Top := 0;
  r.Bottom := Self.Height;

  Self.GetPictureSize(w);

  if w <> 0 then Self.DrawPicture(r);

  if FShowCaption then
  begin
    Text := Caption;

    if Text <> '' then
    begin
      Canvas.Font := Self.Font;
      Canvas.Brush.Style := bsClear;
      Canvas.TextRect(r, Text, [tfCenter, tfVerticalCenter, tfSingleLine]);
    end;
  end;
end;

procedure TFsCustomButton.GetContentDimension(out dim: TSize);
var
  TextSize: TSize;
  ImageSize: TSize;
begin
  dim.cx := 0;
  dim.cy := 0;

  if FShowCaption and (Self.Caption <> '') then
  begin
    Self.Canvas.Font := Self.Font;
    TextSize := Self.Canvas.TextExtent(Self.Caption);
  end
  else begin
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;

  ImageSize.cy := Self.GetPictureSize(ImageSize.cx);

  if (ImageSize.cx = 0) and (TextSize.cx = 0) then Exit;

  case FLayout of
    blPictureLeft, blPictureRight:
      begin
        if ImageSize.cx > 0 then
        begin
          if TextSize.cx > 0 then dim.cx := ImageSize.cx + FSpace + TextSize.cx
          else dim.cx := ImageSize.cx;
        end
        else begin
          if TextSize.cx > 0 then dim.cx := TextSize.cx
          else dim.cx := 0;
        end;

        if TextSize.cy > ImageSize.cy then dim.cy := TextSize.cy
        else dim.cy := ImageSize.cy;
      end;

    blPictureTop, blPictureBottom:
      begin
        if ImageSize.cy > 0 then
        begin
          if TextSize.cy > 0 then dim.cy := ImageSize.cy + FSpace + TextSize.cy
          else dim.cy := ImageSize.cy;
        end
        else begin
          if TextSize.cy > 0 then dim.cy := TextSize.cy
          else dim.cy := 0;
        end;

        if TextSize.cx > ImageSize.cx then dim.cx := TextSize.cx
        else dim.cx := ImageSize.cx;
      end;

    blTextInPicture, blTextInStretchPicture:
      begin
        if TextSize.cx > ImageSize.cx then dim.cx := TextSize.cx
        else dim.cx := ImageSize.cx;

        if TextSize.cy > ImageSize.cy then dim.cy := TextSize.cy
        else dim.cy := ImageSize.cy;
      end;
  end;
end;

procedure TFsCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button = mbLeft then Include(FMouseFlag, mfLButtonDown);

  if Button = mbRight then Include(FMouseFlag, mfRButtonDown);

  if FAllowDown then SetDown(True);

  Self.AutoSizeAndInvalidate;
end;

procedure TFsCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  
  if Button = mbLeft then Exclude(FMouseFlag, mfLButtonDown);

  if Button = mbRight then Exclude(FMouseFlag, mfRButtonDown);

  Self.AutoSizeAndInvalidate;
end;

procedure TFsCustomButton.Paint;
begin
  Self.DrawImageAndText;
end;

function TFsCustomButton.PictureIsHorzStretchable: Boolean;
begin
  Result := False;
end;

function TFsCustomButton.PictureIsVertStretchable: Boolean;
begin
  Result := False;
end;

procedure TFsCustomButton.SetAllowDown(const Value: Boolean);
begin
  FAllowDown := Value;
end;

procedure TFsCustomButton.SetDown(const Value: Boolean);
begin
  if FAllowDown and (FDown <> Value) then
  begin
    FDown := Value;
    Self.AutoSizeAndInvalidate;

    if FDown then Self.SetOtherUp;
  end;
end;

procedure TFsCustomButton.SetGroup(const Value: Integer);
begin
  FGroup := Value;
end;

procedure TFsCustomButton.SetLayout(const Value: TFsImageButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    AutoSizeAndInvalidate;
  end;
end;

procedure TFsCustomButton.SetOtherUp;
var
  p: TWinControl;
  i: Integer;
begin
  p := Self.Parent;

  if Assigned(p) then
  begin
    for i := 0 to p.ControlCount - 1 do
    begin
      if (p.Controls[i] <> Self) and (p.Controls[i] is TFsCustomButton) then
        TFsCustomButton(p.Controls[i]).Down := False;
    end;
  end;
end;

procedure TFsCustomButton.SetSpace(const Value: Integer);
begin
  if (FSpace <> Value) and (Value >= 0) then
  begin
    FSpace := Value;
    AutoSizeAndInvalidate;
  end;
end;

procedure TFsCustomButton.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    AutoSizeAndInvalidate;
  end;
end;

{ TFsImageButton }

constructor TFsImageButton.Create(Owner: TComponent);
begin
  inherited;
  FPicture := TFsPicture.Create;
  FDisablePicture := TFsPicture.Create;
  FMouseOverPicture := TFsPicture.Create;
  FMouseDownPicture := TFsPicture.Create;
  
  FPicture.OnChange := Self.PictureChanged;
  FDisablePicture.OnChange := Self.PictureChanged;
  FMouseOverPicture.OnChange := Self.PictureChanged;
  FMouseDownPicture.OnChange := Self.PictureChanged;
end;

destructor TFsImageButton.Destroy;
begin
  FPicture.Free;
  FDisablePicture.Free;
  FMouseOverPicture.Free;
  FMouseDownPicture.Free;
  inherited;
end;

procedure TFsImageButton.DrawPicture(const Rect: TRect);
var
  drawable: TFsPicture;
begin
  drawable := Self.GetDrawable;

  if Assigned(drawable) and (drawable.Picture.Width > 0) then
    drawable.Draw(Self.Canvas, Rect);
end;

function TFsImageButton.GetDrawable: TFsPicture;
begin
  if not Self.Enabled then Result := FDisablePicture
  else if FAllowDown and FDown then Result := FMouseDownPicture       
  else if mfLButtonDown in FMouseFlag then Result := FMouseDownPicture
  else if mfMouseOver in FMouseFlag then Result := FMouseOverPicture
  else Result := FPicture;

  if Result.Picture.Width = 0 then Result := FPicture;
end;

function TFsImageButton.GetPictureSize(out width: Integer): Integer;
var
  drawable: TFsPicture;
begin
  drawable := Self.GetDrawable;

  width := drawable.Picture.Width;
  Result := drawable.Picture.Height;
end;

procedure TFsImageButton.PictureChanged(Sender: TObject);
begin
  if Sender = GetDrawable then Self.AutoSizeAndInvalidate;
end;

procedure TFsImageButton.SetPicture(const Value: TFsPicture);
begin
  FPicture.Assign(Value);
end;

procedure TFsImageButton.SetDisablePicture(const Value: TFsPicture);
begin
  FDisablePicture.Assign(Value);
end;

procedure TFsImageButton.SetMouseDownPicture(const Value: TFsPicture);
begin
  FMouseDownPicture.Assign(Value);
end;

procedure TFsImageButton.SetMouseOverPicture(const Value: TFsPicture);
begin
  FMouseOverPicture.Assign(Value);
end;

{ TFsImage }

constructor TFsImage.Create(Owner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable, csPannable];
  Self.SetBounds(Left, Top, 100, 100);
end;

destructor TFsImage.Destroy;
begin
  SetPicture(nil);
  inherited;
end;

procedure TFsImage.GetContentDimension(out dim: TSize);
begin
  if Assigned(FPicture) then
  begin
    dim.cx := FPicture.Width;
    dim.cy := FPicture.Height;
  end
  else begin
    dim.cx := 0;
    dim.cy := 0;
  end;
end;

procedure TFsImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FPicture then
    begin
      FPicture := nil;
      Self.AutoSizeAndInvalidate;
    end;
  end;
end;

procedure TFsImage.Paint;
var
  r: TRect;
begin
  inherited;

  r := Rect(0, 0, Width, Height);

  if Assigned(FPicture) and not FPicture.Empty then
    TFsDrawable(FPicture).Draw(Canvas, r);

  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TFsImage.PictureChanged(Sender: TObject);
begin
  if Sender = FPicture then Self.AutoSizeAndInvalidate;
end;

procedure TFsImage.SetPicture(const Value: TFsDrawable);
begin
  if FPicture <> Value then
  begin
    if Assigned(FPicture) then
    begin
      FPicture.RemoveOnChangeListener(Self.PictureChanged);
      FPicture.RemoveFreeNotification(Self);
    end;

    FPicture := Value;

    if Assigned(FPicture) then
    begin
      FPicture.AddOnChangeListener(Self.PictureChanged);
      FPicture.FreeNotification(Self);
    end;
      
    Self.AutoSizeAndInvalidate;
  end;
end;

{ TFsCustomCheckBox }

procedure TFsCustomCheckBox.CMFontChange(var msg: TMessage);
begin
  Self.AutoSizeAndInvalidate;
end;

procedure TFsCustomCheckBox.CMTextChange(var msg: TMessage);
begin
  Self.AutoSizeAndInvalidate;
end;

constructor TFsCustomCheckBox.Create(Owner: TComponent);
begin
  inherited;
  FSpace := 4;
  ControlStyle := ControlStyle + [csReplicatable, csPannable];
end;

procedure TFsCustomCheckBox.GetContentDimension(out dim: TSize);
var
  TextSize: TSize;
  ImageWidth, ImageHeight: Integer;
begin
  Canvas.Font := Self.Font;

  TextSize := Canvas.TextExtent(Self.Caption);

  Self.GetImageSize(ImageWidth, ImageHeight);

  if ImageWidth = 0 then
  begin
    ImageWidth := Windows.GetSystemMetrics(SM_CXMENUCHECK);
    ImageHeight := Windows.GetSystemMetrics(SM_CYMENUCHECK);
  end;

  dim.cx := ImageWidth + FSpace + TextSize.cx;

  if TextSize.cy > ImageHeight then dim.cy := TextSize.cy
  else dim.cy := ImageHeight;
end;

procedure TFsCustomCheckBox.GetImageSize(out w, h: Integer);
begin
  w := 0;
  h := 0;
end;

procedure TFsCustomCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FChecked := not FChecked;
    Invalidate;
  end;

  inherited;
end;

procedure TFsCustomCheckBox.Paint;
var
  w, h: Integer;
  r: TRect;
  _caption: string;
begin
  inherited;

  Self.GetImageSize(w, h);

  if w = 0 then
  begin
    w := Windows.GetSystemMetrics(SM_CXMENUCHECK);
    h := Windows.GetSystemMetrics(SM_CYMENUCHECK);

    r.Left := 0;
    r.Top := (Self.Height - h) div 2;
    r.Right := w;
    r.Bottom := r.Top + h;

    if Self.FChecked then
      Windows.DrawFrameControl(Canvas.Handle, r, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED)
    else
      Windows.DrawFrameControl(Canvas.Handle, r, DFC_BUTTON, DFCS_BUTTONCHECK);
  end
  else begin
    r.Left := 0;
    r.Top := (Self.Height - h) div 2;
    r.Right := w;
    r.Bottom := r.Top + h;
    Self.DrawMark(r);
  end;

  r := Rect(r.Right + FSpace, 0, Self.Width, Self.Height);

  Self.Canvas.Font := Self.Font;
  Self.Canvas.Brush.Style := bsClear;
  SetBkMode(Self.Canvas.Handle, Windows.TRANSPARENT);
  _caption := Self.Caption;
  
  Self.Canvas.TextRect(r, _caption, [tfVerticalCenter, tfSingleLine]);
end;

procedure TFsCustomCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    AutoSizeAndInvalidate;
  end;
end;

procedure TFsCustomCheckBox.SetSpace(const Value: Integer);
begin
  if (FSpace <> Value) and (Value >= 0) then
  begin
    FSpace := Value;
    Self.AutoSizeAndInvalidate;
  end;
end;

{ TFsCheckBox }

constructor TFsCheckBox.Create(Owner: TComponent);
begin
  inherited;
  FCheckedPicture := TFsPicture.Create;
  FUnCheckedPicture := TFsPicture.Create;
  FCheckedPicture.OnChange := Self.PictureChanged;
  FUnCheckedPicture.OnChange := Self.PictureChanged;
end;

destructor TFsCheckBox.Destroy;
begin
  FCheckedPicture.Free;
  FUnCheckedPicture.Free;
  inherited;
end;

procedure TFsCheckBox.DrawMark(const Rect: TRect);
begin
  if Self.Checked then
    FCheckedPicture.Draw(Self.Canvas, Rect)
  else
    FUnCheckedPicture.Draw(Self.Canvas, Rect)
end;

procedure TFsCheckBox.GetImageSize(out w, h: Integer);
begin
  if Self.Checked then
  begin
    w := FCheckedPicture.Picture.Width;
    h := FCheckedPicture.Picture.Height;
  end
  else begin
    w := FUnCheckedPicture.Picture.Width;
    h := FUnCheckedPicture.Picture.Height;
  end;
end;

procedure TFsCheckBox.PictureChanged(Sender: TObject);
begin
  if ((Sender = FCheckedPicture) and FChecked) or ((Sender = FUnCheckedPicture) and not FChecked) then
    Self.AutoSizeAndInvalidate;
end;

procedure TFsCheckBox.SetCheckedPicture(const Value: TFsPicture);
begin
  FCheckedPicture.Assign(Value);
end;

procedure TFsCheckBox.SetUnCheckedPicture(const Value: TFsPicture);
begin
  FUnCheckedPicture.Assign(Value);
end;

{ TFsCoverButton }

constructor TFsCoverButton.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TFsPicture.Create;
  FPicture.OnChange := Self.PictureChanged;
end;

destructor TFsCoverButton.Destroy;
begin
  FPicture.Free;
  SetDownCover(nil);
  SetHoverCover(nil);
  inherited;
end;

procedure TFsCoverButton.DrawPicture(const Rect: TRect);
begin
  if FPicture.Picture.Width > 0 then
    FPicture.Draw(Self.Canvas, Rect);
end;

function TFsCoverButton.GetCover: TFsDrawable;
begin
  inherited;

  if AllowDown and Down then Result := FDownCover
  else if mfLButtonDown in FMouseFlag then Result := FDownCover
  else if mfMouseOver in FMouseFlag then Result := FHoverCover
  else Result := nil;
end;

function TFsCoverButton.GetPictureSize(out width: Integer): Integer;
begin
  width := FPicture.Picture.Width;
  Result := FPicture.Picture.Height;
end;

procedure TFsCoverButton.LinkedPictureChanged(Sender: TObject);
begin
  if Sender = GetCover then Self.AutoSizeAndInvalidate;
end;

procedure TFsCoverButton.Notification(AComponent: TComponent; Operation: TOperation);
var
  changed: Boolean;
begin
  inherited;

  changed := False;

  if Operation = opRemove then
  begin
    if AComponent = FDownCover then
    begin
      FDownCover := nil;
      changed := True;
    end;

    if AComponent = FHoverCover then
    begin
      FHoverCover := nil;
      changed := True;
    end;
  end;

  if changed then Self.AutoSizeAndInvalidate;
end;

procedure TFsCoverButton.Paint;
var
  cover: TFsDrawable;
  dim: TSize;
  w, h: Integer;
  r: TRect;
begin
  Self.GetContentDimension(dim);
  cover := Self.GetCover;

  if Assigned(cover) then
  begin
    w := cover.Width;

    if (w < dim.cx) and cover.HorzSafeStretch then w := dim.cx;

    h := cover.Height;

    if (h < dim.cy) and cover.VertSafeStretch then h := dim.cy;

    r.Left := (Self.Width - w) div 2;
    r.Right := r.Left + w;
    r.Top := (Self.Height - h) div 2;
    r.Bottom := r.Top + h;

    cover.Draw(Self.Canvas, r);
  end;

  inherited;
end;

procedure TFsCoverButton.PictureChanged(Sender: TObject);
begin
  Self.AutoSizeAndInvalidate;
end;

procedure TFsCoverButton.SetDownCover(const Value: TFsDrawable);
begin
  if FDownCover <> Value then
  begin
    if Assigned(FDownCover) then
    begin
      FDownCover.RemoveOnChangeListener(Self.LinkedPictureChanged);
      FDownCover.RemoveFreeNotification(Self);
    end;

    FDownCover := Value;

    if Assigned(FDownCover) then
    begin
      FDownCover.AddOnChangeListener(Self.LinkedPictureChanged);
      FDownCover.FreeNotification(Self);
    end;

    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsCoverButton.SetHoverCover(const Value: TFsDrawable);
begin
  if FHoverCover <> Value then
  begin
    if Assigned(FHoverCover) then
    begin
      FHoverCover.RemoveOnChangeListener(Self.LinkedPictureChanged);
      FHoverCover.RemoveFreeNotification(Self);
    end;

    FHoverCover := Value;

    if Assigned(FHoverCover) then
    begin
      FHoverCover.AddOnChangeListener(Self.LinkedPictureChanged);
      FHoverCover.FreeNotification(Self);
    end;

    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsCoverButton.SetPicture(const Value: TFsPicture);
begin
  FPicture.Assign(Value);
end;

{ TFsMemo }

function TFsMemo.CreateRealControl: TControl;
begin
  Result := TFsBorderlessMemo.Create(Self);
  TFsBorderlessMemo(Result).ScrollBars := ssVertical;
end;

function TFsMemo.GetLines: TStrings;
begin
  Result := TFsBorderlessMemo(RealControl).Lines;
end;

function TFsMemo.GetMemo: TCustomMemo;
begin
  Result := TCustomMemo(RealControl);
end;

function TFsMemo.GetScrollBars: TScrollStyle;
begin
  Result := TFsBorderlessMemo(RealControl).ScrollBars;
end;

procedure TFsMemo.SetLines(const Value: TStrings);
begin
  TFsBorderlessMemo(RealControl).Lines := Value;
end;

procedure TFsMemo.SetScrollBars(const Value: TScrollStyle);
begin
  TFsBorderlessMemo(RealControl).ScrollBars := Value;
end;

procedure BorderLesssWndProc(control: TWinControl; InheritedProc: TWndMethod; var msgr: TMessage);
var
  hwnd: THandle;
  vsi1, hsi1, vsi2, hsi2: TScrollInfo;
  vshow1, hshow1, vshow2, hshow2, changed: Boolean;
  style: Integer;
begin
  if not control.HandleAllocated or (msgr.Msg = WM_CREATE) or (msgr.Msg = WM_NCCREATE)
    or (msgr.Msg = WM_DESTROY) or (msgr.Msg = WM_NCDESTROY) then
  begin
    InheritedProc(msgr);
    Exit;
  end;

  hwnd := control.Handle;
  changed := False;

  style := GetWindowLong(hwnd, GWL_STYLE);

  if style and WS_VSCROLL <> 0 then
  begin
    vsi1.cbSize := SizeOf(vsi1);
    vsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    GetScrollInfo(hwnd, SB_VERT, vsi1);
    vshow1 := NeedScroll(vsi1);
  end
  else vshow1 := False;

  if style and WS_HSCROLL <> 0 then
  begin
    hsi1.cbSize := SizeOf(hsi1);
    hsi1.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    GetScrollInfo(hwnd, SB_HORZ, hsi1);
    hshow1 := NeedScroll(hsi1);
  end
  else hshow1 := False;

  InheritedProc(msgr);

  hwnd := Control.Handle;

  style := GetWindowLong(hwnd, GWL_STYLE);

  if style and WS_VSCROLL <> 0 then
  begin
    vsi2.cbSize := SizeOf(vsi2);
    vsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    GetScrollInfo(hwnd, SB_VERT, vsi2);
    vshow2 := NeedScroll(vsi2);
  end
  else vshow2 := False;

  if (vshow1 <> vshow2) or ( vshow1 and ( (vsi1.nMin <> vsi2.nMin) or (vsi1.nMax <> vsi2.nMax)
      or (vsi1.nPage <> vsi2.nPage) or (vsi1.nPos <> vsi2.nPos) ) ) then
    changed := True;

  if style and WS_HSCROLL <> 0 then
  begin
    hsi2.cbSize := SizeOf(hsi2);
    hsi2.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    GetScrollInfo(hwnd, SB_HORZ, hsi2);

    hshow2 := NeedScroll(hsi2);
  end
  else hshow2 := False;

  if (hshow1 <> hshow2) or ( hshow1 and ( (hsi1.nMin <> hsi2.nMin) or (hsi1.nMax <> hsi2.nMax)
      or (hsi1.nPage <> hsi2.nPage) or (hsi1.nPos <> hsi2.nPos) ) ) then
    changed := True;

  if changed and (control.Parent is TFsScrollContainer) then
  begin
    if (vshow1 <> vshow2) or (hshow1 <> hshow2) then TFsScrollContainer(control.Parent).NCChanged
    else TFsScrollContainer(control.Parent).PaintNC;
  end;
end;

{ TFsBorderlessMemo }

procedure TFsBorderlessMemo.InheritedWndProc(var msgr: TMessage);
begin
  inherited WndProc(msgr);
end;

procedure TFsBorderlessMemo.WMNCCalcSize(var msgr: TWMNCCalcSize);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessMemo.WMNCPaint(var msgr: TWMNCPaint);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessMemo.WndProc(var msgr: TMessage);
begin
  BorderLesssWndProc(Self, Self.InheritedWndProc, msgr);
end;

{ TFsListBox }

function TFsListBox.CreateRealControl: TControl;
begin
  Result := TFsBorderlessListBox.Create(Self);
end;

function TFsListBox.GetItems: TStrings;
begin
  Result := TFsBorderlessListBox(RealControl).Items;
end;

function TFsListBox.GetListBox: TCustomListBox;
begin
  Result := TCustomListBox(RealControl);
end;

procedure TFsListBox.SetItems(const Value: TStrings);
begin
  TFsBorderlessListBox(RealControl).Items := Value;
end;

{ TFsBorderlessListBox }

procedure TFsBorderlessListBox.InheritedWndProc(var msgr: TMessage);
begin
  inherited WndProc(msgr);
end;

procedure TFsBorderlessListBox.WMNCCalcSize(var msgr: TWMNCCalcSize);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessListBox.WMNCPaint(var msgr: TWMNCPaint);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessListBox.WndProc(var msgr: TMessage);
begin
  BorderLesssWndProc(Self, Self.InheritedWndProc, msgr);
end;

{ TFsBorderlessListView }

procedure TFsBorderlessListView.InheritedWndProc(var msgr: TMessage);
begin
  inherited WndProc(msgr);
end;

procedure TFsBorderlessListView.WMNCCalcSize(var msgr: TWMNCCalcSize);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessListView.WMNCPaint(var msgr: TWMNCPaint);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessListView.WndProc(var msgr: TMessage);
begin
  BorderLesssWndProc(Self, Self.InheritedWndProc, msgr);
end;

{ TFsListView }

function TFsListView.CreateRealControl: TControl;
begin
  Result := TFsBorderlessListView.Create(Self);
end;

function TFsListView.GetColumns: TListColumns;
begin
  Result := TFsBorderlessListView(RealControl).Columns;
end;

function TFsListView.GetItems: TListItems;
begin
  Result := TFsBorderlessListView(RealControl).Items;
end;

function TFsListView.GetListView: TCustomListView;
begin
  Result := TCustomListView(RealControl);
end;

function TFsListView.GetViewStyle: TViewStyle;
begin
  Result := TFsBorderlessListView(RealControl).ViewStyle;
end;

procedure TFsListView.SetColumns(const Value: TListColumns);
begin
  TFsBorderlessListView(RealControl).Columns := Value;
end;

procedure TFsListView.SetItems(const Value: TListItems);
begin
  TFsBorderlessListView(RealControl).Items := Value;
end;

procedure TFsListView.SetViewStyle(const Value: TViewStyle);
begin
  TFsBorderlessListView(RealControl).ViewStyle := Value;
end;

{ TFsBorderlessTreeView }

procedure TFsBorderlessTreeView.InheritedWndProc(var msgr: TMessage);
begin
  inherited WndProc(msgr);
end;

procedure TFsBorderlessTreeView.WMNCCalcSize(var msgr: TWMNCCalcSize);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessTreeView.WMNCPaint(var msgr: TWMNCPaint);
begin
  msgr.Result := 0;
end;

procedure TFsBorderlessTreeView.WndProc(var msgr: TMessage);
begin
  BorderLesssWndProc(Self, Self.InheritedWndProc, msgr);
end;

{ TFsTreeView }

function TFsTreeView.CreateRealControl: TControl;
begin
  Result := TFsBorderlessTreeView.Create(Self);
end;

function TFsTreeView.GetItems: TTreeNodes;
begin
  Result := TFsBorderlessTreeView(RealControl).Items;
end;

function TFsTreeView.GetTreeView: TCustomTreeView;
begin
  Result := TCustomTreeView(RealControl);
end;

procedure TFsTreeView.SetItems(const Value: TTreeNodes);
begin
  TFsBorderlessTreeView(RealControl).Items := Value;
end;

{ TFsPanel }

procedure TFsPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  VerticalAlignments: array[TVerticalAlignment] of Longint = (DT_TOP, DT_BOTTOM, DT_VCENTER);
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  Flags: Longint;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;

  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;

  if csParentBackground in ControlStyle then
    Frame3D(Canvas, Rect, Color, Color, BorderWidth)
  else
    InflateRect(Rect, -BorderWidth, -BorderWidth);

  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  
  with Canvas do
  begin
    if not ParentBackground then
    begin
      Brush.Color := Color;
      FillRect(Rect);
    end;

    Brush.Style := bsClear;
    Font := Self.Font;

    Flags := DT_EXPANDTABS or DT_SINGLELINE or
      VerticalAlignments[Self.VerticalAlignment] or Alignments[Self.Alignment];
      
    Flags := DrawTextBiDiModeFlags(Flags);
    DrawText(Handle, PChar(Caption), -1, Rect, Flags);
  end;
end;

procedure TFsPanel.WMEraseBkgnd(var msgr: TWMEraseBkgnd);
begin
  if not FDoubleBuffered or (msgr.DC = HDC(msgr.Unused)) then
  begin
    if ParentBackground and Assigned(Parent) then
      ThemeServices.DrawParentBackground(Handle, msgr.DC, nil, False)
    else
      Windows.FillRect(msgr.DC, ClientRect, Self.Brush.Handle);
  end;
  msgr.Result := 1;
end;

{ TFsPageControl }

procedure TFsPageControl.WMNCCalcSize(var msgr: TWMNCCalcSize);
begin
  with msgr.CalcSize_Params.rgrc[0] do
    OutputDebugString(PChar(Format('%d, %d, %d, %d', [Left, Top, Right, Bottom])));

  inherited;

  with msgr.CalcSize_Params.rgrc[0] do
    OutputDebugString(PChar(Format('%d, %d, %d, %d', [Left, Top, Right, Bottom])));
end;

{ TFsCustomCombobox }

procedure TFsCustomCombobox.AdjustEditBoundsRect;
begin
  case Self.Style of
    csDropDown: MoveWindow(FEditHandle, 1, 1, Self.Width - 2 - FButtonWidth, Self.Height - 2, True);
    csSimple: MoveWindow(FEditHandle, 1, 1, Self.Width - 2, Self.Height - 2, True);
  end;
end;

procedure TFsCustomCombobox.CalcEditBoundsRect(var r: TRect);
begin
  r.Left := 1;
  r.Top := 1;
  case Self.Style of
    csDropDown:
      begin
        r.Right := r.Left + Self.Width - 2 - FButtonWidth;
        r.Bottom := r.Top + Self.Height - 2;
      end;
    csSimple:
      begin
        r.Right := r.Left + Self.Width - 2;
        r.Bottom := r.Top + Self.Height - 2;
      end
  end;
end;

procedure TFsCustomCombobox.CBShowShopDown(var msgr: TMessage);
begin
  inherited;
  AdjustEditBoundsRect;
  Self.Invalidate;
end;

procedure TFsCustomCombobox.CheckInvalidate;
begin
  if (FMouseInControlLastPaint <> MouseInControl) and Visible then
    Self.Paint;
end;

procedure TFsCustomCombobox.CMMouseEnter(var msgr: TMessage);
begin
  inherited;
  CheckInvalidate;
end;

procedure TFsCustomCombobox.CMMouseLeave(var msgr: TMessage);
begin
  inherited;
  CheckInvalidate;
end;

constructor TFsCustomCombobox.Create(AOwner: TComponent);
begin
  inherited;
  FButtonWidth := 24;
  FBorderColorHover := RGB(123, 228, 255);
  FBorderColor := RGB(78, 160, 209);
  FTipFont := TFont.Create;
  FTipFont.Color := clGrayText;
  FTipFont.OnChange := Self.TipFontChanged;
  FTextFont := TFont.Create;
  FTextFont.OnChange := Self.TextFontChanged;
  Font.Assign(FTextFont);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

procedure TFsCustomCombobox.CreateParams(var Params: TCreateParams);
begin
  inherited;

  if FNumberOnly then Params.Style := Params.Style or ES_NUMBER
  else Params.Style := Params.Style and not ES_NUMBER;
end;

procedure TFsCustomCombobox.CreateWnd;
begin
  inherited;
  AdjustEditBoundsRect;
end;

destructor TFsCustomCombobox.Destroy;
begin
  FCanvas.Free;
  FTipFont.Free;
  FTextFont.Free;
  inherited;
end;

procedure TFsCustomCombobox.DoEnter;
begin
  ShowingTip := False;
  inherited;
end;

procedure TFsCustomCombobox.DoExit;
begin
  if Text = '' then ShowingTip := True;
  inherited;
end;

procedure TFsCustomCombobox.EditWndProc(var msgr: TMessage);
var
  tme: TTrackMouseEvent;
  pwp: PWindowPos;
  handled: Boolean;
  r: TRect;
begin
  handled := False;

  case msgr.Msg of
    WM_MOUSEMOVE:
      if not FIsEditTrackingMouse then
      begin
        tme.cbSize := SizeOf(tme);
        tme.dwFlags := TME_LEAVE;
        tme.hwndTrack := FEditHandle;
        tme.dwHoverTime := 5;
        Windows.TrackMouseEvent(tme);
        FIsEditTrackingMouse := True;
      end;

    WM_MOUSELEAVE:
      begin
        FIsEditTrackingMouse := False;
        CheckInvalidate;
      end;

    WM_WINDOWPOSCHANGING:
      begin
        CalcEditBoundsRect(r);
        pwp := PWindowPos(msgr.LParam);
        pwp.x := r.Left;
        pwp.y := r.Top;
        pwp.cx := r.Right - r.Left;
        pwp.cy := r.Bottom - r.Top;
      end;
  end;

  if not handled then inherited;
end;

function TFsCustomCombobox.GetText: string;
begin
  if FShowingTip then Result := ''
  else Result := inherited Text;
end;

procedure TFsCustomCombobox.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    SetShowingTip(Text = '');
  end;
end;

function TFsCustomCombobox.MouseInControl: Boolean;
var
  pt: TPoint;
  rw: TRect;
begin
  GetCursorPos(pt);
  GetWindowRect(WindowHandle, rw);

  Result := PtInRect(rw, pt);  
end;

procedure TFsCustomCombobox.Paint;
var
  bgc: TColor;
  r, r1: TRect;
  _text: string;
begin
  FMouseInControlLastPaint := MouseInControl;

  if FMouseInControlLastPaint then bgc := FBorderColorHover
  else bgc := FBorderColor;

  r.Left := 0;
  r.Top := 0;
  r.Right := Self.Width;
  r.Bottom := Self.Height;

  FCanvas.Brush.Color := Self.Color;

  if FEditHandle = 0 then
  begin
    FCanvas.Brush.Color := Self.Color;
    FCanvas.Brush.Style := bsSolid;

    FCanvas.Pen.Width := 1;
    FCanvas.Pen.Color := bgc;
    FCanvas.Pen.Style := psSolid;

    FCanvas.Rectangle(r);

    r1.Left := 2;
    r1.Right := r.Right - FButtonWidth;
    r1.Top := 1;
    r1.Bottom := Self.ClientHeight - 1;

    FCanvas.Font := Self.Font;

    _text := Self.Text;

    FCanvas.TextRect(r1, _text, [tfSingleLine, tfVerticalCenter]);
  end
  else begin
    FCanvas.Brush.Color := bgc;
    FCanvas.Brush.Style := bsSolid;
    FCanvas.FrameRect(r);
    r.Right := Self.Width - 1;
    r.Left := r.Right - FButtonWidth ;
    r.Top := 1;
    r.Bottom := Self.Height - 1;
    FCanvas.Brush.Color := Self.Color;
    FCanvas.FillRect(r);
  end;

  r.Right := Self.Width - 1;
  r.Left := r.Right - FButtonWidth;
  r.Top := 1;
  r.Bottom := Self.ClientHeight - 1;

  Self.DrawButton(r);
end;

procedure TFsCustomCombobox.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TFsCustomCombobox.SetBorderColorHover(const Value: TColor);
begin
  if FBorderColorHover <> Value then
  begin
    FBorderColorHover := Value;
    Invalidate;
  end;
end;

procedure TFsCustomCombobox.SetButtonWidth(const Value: Integer);
begin
  if (FButtonWidth <> Value) and (Value > 0) then
  begin
    FButtonWidth := Value;
    AdjustEditBoundsRect;
  end;
end;

procedure TFsCustomCombobox.SetNumberOnly(const Value: Boolean);
var
  style: DWORD;
begin
  FNumberOnly := Value;

  if HandleAllocated then
  begin
    style := GetWindowLong(WindowHandle, GWL_STYLE);

    if FNumberOnly then style := style or ES_NUMBER
    else style := style and not ES_NUMBER;

    SetWindowLong(WindowHandle, GWL_STYLE, style);
  end;
end;

procedure TFsCustomCombobox.SetShowingTip(const Value: Boolean);
begin
  if FShowingTip <> Value then
  begin
    FShowingTip := Value;

    if ShowingTip then
    begin
      if FShowTip then
      begin
        inherited Text := FTip;
        Font.Assign(FTipFont);
      end;
    end
    else begin
      inherited Text := '';
      Font.Assign(FTextFont);
    end;
  end;
end;

procedure TFsCustomCombobox.SetShowTip(const Value: Boolean);
begin
  if FShowTip <> Value then
  begin
    FShowTip := Value;

    if not (csLoading in ComponentState) then
    begin
      if ShowTip then
      begin
        if Text = '' then  SetShowingTip(True);
      end
      else SetShowingTip(False);
    end;
  end;
end;

procedure TFsCustomCombobox.SetText(const Value: string);
begin
  ShowingTip := Value = '';

  if not FShowingTip then inherited Text := Value;
end;

procedure TFsCustomCombobox.SetTextFont(const Value: TFont);
begin
  FTextFont.Assign(Value);
end;

procedure TFsCustomCombobox.SetTip(const Value: string);
begin
  FTip := Value;
  if FShowingTip then inherited Text := FTip;
end;

procedure TFsCustomCombobox.SetTipFont(const Value: TFont);
begin
  FTipFont.Assign(Value);
end;

function TFsCustomCombobox.ShouldStoreText: Boolean;
begin
  Result := not FShowingTip;
end;

procedure TFsCustomCombobox.TextFontChanged(Sender: TObject);
begin
  if not FShowingTip then Font.Assign(FTextFont);  
end;

procedure TFsCustomCombobox.TipFontChanged(Sender: TObject);
begin
  if FShowingTip then Font.Assign(FTipFont);
end;

procedure TFsCustomCombobox.WMLButtonDblClk(var msgr: TWMLButtonDblClk);
begin
  msgr.Result := 0;  
end;

procedure TFsCustomCombobox.WMLButtonDown(var msgr: TWMNCLButtonDown);
begin
  SendMessage(Handle, CB_SHOWDROPDOWN, Longint(not Self.DroppedDown), 0);
  msgr.Result := 0;
end;

procedure TFsCustomCombobox.WMLButtonUp(var msgr: TWMNCLButtonUp);
begin
  msgr.Result := 0;
end;

procedure TFsCustomCombobox.WMNCCalcSize(var msgr: TWMNCCalcSize);
begin
  msgr.Result := 0;
end;

procedure TFsCustomCombobox.WMPaint(var msgr: TWMPaint);
var
  ps: TPaintStruct;
  dc: HDC;
begin
  dc := BeginPaint(Handle, ps);

  FCanvas.Lock;
  
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
    EndPaint(Handle, ps);
    msgr.Result := 0;
  end;
end;

procedure TFsCustomCombobox.WMSize(var msgr: TWMSize);
begin
  inherited;
  AdjustEditBoundsRect;
end;

{ TFsCombobox }

procedure TFsCombobox.ButtonPictureChanged(Sender: TObject);
begin
  Self.Invalidate;
end;

constructor TFsCombobox.Create(AOwner: TComponent);
begin
  inherited;
  FButtonPicture := TFsPicture.Create;
  FButtonPicture.OnChange := Self.ButtonPictureChanged;
end;

destructor TFsCombobox.Destroy;
begin
  FButtonPicture.Free;
  inherited;
end;

procedure TFsCombobox.DrawButton(const Rect: TRect);
var
  x, y: Integer;
begin
  if FButtonPicture.Picture.Width > 0 then
  begin
    x := Rect.Left + (Rect.Right - Rect.Left - FButtonPicture.Picture.Width) div 2;
    y := Rect.Top + (Rect.Bottom - Rect.Top - FButtonPicture.Picture.Height) div 2;
    Canvas.Draw(x, y, FButtonPicture.Picture.Graphic);
  end;
end;

procedure TFsCombobox.SetButtonPicture(const Value: TFsPicture);
begin
  FButtonPicture.Assign(Value);
end;

end.
