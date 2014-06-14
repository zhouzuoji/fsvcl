unit FSNavTree;

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics, Controls, FSVclBase, FSGraphics, FSControls;

type
  TFsNavTree = class;
  TFsNavTreeNodes = class;

  TFsNavTreeCategory = class(TCollectionItem)
  private
    FText: string;
    FNodes: TFsNavTreeNodes;
    FImage: TFsPicture;
    FHeight: Integer;
    procedure SetText(const Value: string);
    procedure SetNodes(const Value: TFsNavTreeNodes);
    procedure SetImage(const Value: TFsPicture);
    procedure SetHeight(const Value: Integer);
  protected
    procedure ImageChanged(Sender: TObject);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read FText write SetText;
    property Nodes: TFsNavTreeNodes read FNodes write SetNodes;
    property Image: TFsPicture read FImage write SetImage;
    property Height: Integer read FHeight write SetHeight;
  end;

  TFsNavTreeCategorys = class(TCollection)
  private
    FTree: TFsNavTree;
    function GetItem(Index: Integer): TFsNavTreeCategory;
    procedure SetItem(Index: Integer; const Value: TFsNavTreeCategory);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ATree: TFsNavTree);
    property Tree: TFsNavTree read FTree;
    property Items[Index: Integer]: TFsNavTreeCategory read GetItem write SetItem; default;
  end;

  TFsNavTreeNode = class(TCollectionItem)
  private
    FText: string;
    FChildNodes: TFsNavTreeNodes;
    FExpanded: Boolean;
    FHeight: Integer;
    procedure SetText(const Value: string);
    procedure SetChildNodes(const Value: TFsNavTreeNodes);
    procedure SetExpanded(const Value: Boolean);
    function GetCategory: TFsNavTreeCategory;
    procedure SetHeight(const Value: Integer);
    function GetLevel: Integer;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Height: Integer read FHeight write SetHeight;
    property Text: string read FText write SetText;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property ChildNodes: TFsNavTreeNodes read FChildNodes write SetChildNodes;
    property Category: TFsNavTreeCategory read GetCategory;
    property Level: Integer read GetLevel;
  end;

  TFsNavTreeNodes = class(TCollection)
  private
    FCategory: TFsNavTreeCategory;
    FParent: TFsNavTreeNode;
    function GetCategory: TFsNavTreeCategory;
    function GetItem(Index: Integer): TFsNavTreeNode;
    procedure SetItem(Index: Integer; const Value: TFsNavTreeNode);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ACategory: TFsNavTreeCategory); reintroduce; overload;
    constructor Create(AParent: TFsNavTreeNode); reintroduce; overload;
    property Category: TFsNavTreeCategory read GetCategory;
    property Parent: TFsNavTreeNode read FParent;
    property Items[Index: Integer]: TFsNavTreeNode read GetItem write SetItem; default;
  end;

  TFsNavTreeDrawInfo = record
    ClientRect: TRect;
    OffsetY: Integer;
    CurrentOffsetY: Integer;
  end;
  PFsNavTreeDrawInfo = ^TFsNavTreeDrawInfo;

  TFsNavTreeHitTest = (htNone, htCategoryImage, htCategoryText, htNodeImage, htNodeText);

  TFsNavTreeHitTestInfo = record
    ClientRect: TRect;
    OffsetY: Integer;
    CurrentOffsetY: Integer;
    HitTest: TFsNavTreeHitTest;
    area: TRect;
    obj: TObject;
  end;

  TNodeClickEvent = procedure(Sender: TObject; node: TFsNavTreeNode) of object;

  TFsNavTree = class(TFsCustomControl)
  private
    FCategorys: TFsNavTreeCategorys;
    FCategoryImageOffset: Integer;
    FCategoryTextOffset: Integer;
    FCategoryFont: TFont;
    FLeafImage: TFsPicture;
    FCollapsedImage: TFsPicture;
    FExpandedImage: TFsPicture;
    FNodeIndent: Integer;
    FNodeTextOffset: Integer;
    FOnClickNode: TNodeClickEvent;
    FHighlightColor: TColor;
    FHighlightNode: TFsNavTreeNode;
    function GetNodePictrue(node: TFsNavTreeNode): TFsPicture;
    function GetNodeImageRect(node: TFsNavTreeNode; const r: TRect): TRect;
    function GetCategoryImageRect(category: TFsNavTreeCategory; const r: TRect): TRect;
    procedure SetCategorys(const Value: TFsNavTreeCategorys);
    function GetVisibleNodeTotalHeight(node: TFsNavTreeNode): Integer;
    function GetContentHeight: Integer;
    function DrawNode(node: TFsNavTreeNode; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
    function DrawCategory(category: TFsNavTreeCategory; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
    function HitTestNode(X, Y: Integer; node: TFsNavTreeNode; var HitTestInfo: TFsNavTreeHitTestInfo): Boolean;
    function HitTestCategory(X, Y: Integer; category: TFsNavTreeCategory; var HitTestInfo: TFsNavTreeHitTestInfo): Boolean;
    procedure SetCategoryImageOffset(const Value: Integer);
    procedure SetCategoryTextOffset(const Value: Integer);
    procedure SetCategoryFont(const Value: TFont);
    procedure SetCollapsedImage(const Value: TFsPicture);
    procedure SetExpandedImage(const Value: TFsPicture);
    procedure SetLeafImage(const Value: TFsPicture);
    procedure SetNodeIndent(const Value: Integer);
    procedure SetNodeTextOffset(const Value: Integer);
    procedure SetHighlightColor(const Value: TColor);
  protected
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DoCanScroll(IsVert: Boolean; var nPos: Integer); override;
    procedure DoClickNode(node: TFsNavTreeNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateScrollRange;
    function HitTest(X, Y: Integer; var area: TRect; var obj: TObject): TFsNavTreeHitTest;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property Ctl3D;
    property Color;
    property Visible;
    property ParentBackground;
    property ParentCtl3D;
    property ScrollBarDrawer;
    property ScrollBars;
    property Font;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor;
    property CollapsedImage: TFsPicture read FCollapsedImage write SetCollapsedImage;
    property ExpandedImage: TFsPicture read FExpandedImage write SetExpandedImage;
    property LeafImage: TFsPicture read FLeafImage write SetLeafImage;
    property NodeIndent: Integer read FNodeIndent write SetNodeIndent;
    property Categorys: TFsNavTreeCategorys read FCategorys write SetCategorys;
    property CategoryFont: TFont read FCategoryFont write SetCategoryFont;
    property CategoryImageOffset: Integer read FCategoryImageOffset write SetCategoryImageOffset;
    property CategoryTextOffset: Integer read FCategoryTextOffset write SetCategoryTextOffset;
    property NodeTextOffset: Integer read FNodeTextOffset write SetNodeTextOffset;
    property MouseWheelMultiple;
    property OnClickNode: TNodeClickEvent read FOnClickNode write FOnClickNode;
  end;    

implementation

{ TFsNavTree }

constructor TFsNavTree.Create(AOwner: TComponent);
begin
  inherited;
  Width := 250;
  Height := 360;
  FCategoryImageOffset := 8;
  FCategoryTextOffset := 10;
  FNodeTextOffset := 5;
  FNodeIndent := 4;
  FHighlightColor := RGB(141, 203, 241);
  FCategoryFont := TFont.Create;
  FCollapsedImage := TFsPicture.Create;
  FExpandedImage := TFsPicture.Create;
  FLeafImage := TFsPicture.Create;
  FCategorys := TFsNavTreeCategorys.Create(Self);
end;

destructor TFsNavTree.Destroy;
begin
  FCategorys.Free;
  FCategoryFont.Free;
  FCollapsedImage.Free;
  FExpandedImage.Free;
  FLeafImage.Free;
  inherited;
end;

procedure TFsNavTree.DoCanScroll(IsVert: Boolean; var nPos: Integer);
begin
  inherited;
  Self.Invalidate;
end;

procedure TFsNavTree.DoClickNode(node: TFsNavTreeNode);
begin
  FHighlightNode := node;

  Self.Invalidate;
  
  if Assigned(FOnClickNode) then
    FOnClickNode(Self, node);
end;

function TFsNavTree.DrawCategory(category: TFsNavTreeCategory; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
var
  r: TRect;
  i: Integer;
begin
  r.Top := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;
  
  Inc(DrawInfo.CurrentOffsetY, category.Height);

  Result := DrawInfo.CurrentOffsetY < DrawInfo.OffsetY + DrawInfo.ClientRect.Bottom - DrawInfo.ClientRect.Top;

  if DrawInfo.CurrentOffsetY > DrawInfo.OffsetY then
  begin
    r.Left := DrawInfo.ClientRect.Left + FCategoryImageOffset;
    r.Right := DrawInfo.ClientRect.Right;
    r.Bottom := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;

    if category.Image.Width > 0 then
    begin
      Canvas.Draw(r.Left, r.Top + (category.Height - category.Image.Height) div 2, category.Image.Picture.Graphic);
      Inc(r.Left, FCategoryTextOffset + category.Image.Width);
    end;

    Canvas.Font := FCategoryFont;
    Windows.SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    Canvas.TextRect(r, category.FText, [tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
  end;

  if Result then
  begin
    for i := 0 to category.Nodes.Count - 1 do
    begin
      if not Self.DrawNode(category.Nodes[i], DrawInfo) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TFsNavTree.DrawNode(node: TFsNavTreeNode; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
var
  r: TRect;
  i: Integer;
  graph: TFsPicture;
begin
  r.Top := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;
  
  Inc(DrawInfo.CurrentOffsetY, node.Height);

  Result := DrawInfo.CurrentOffsetY < DrawInfo.OffsetY + DrawInfo.ClientRect.Bottom - DrawInfo.ClientRect.Top;

  if DrawInfo.CurrentOffsetY > DrawInfo.OffsetY then
  begin
    r.Left := DrawInfo.ClientRect.Left + FCategoryImageOffset + (node.Level + 1) * FNodeIndent;
    r.Right := DrawInfo.ClientRect.Right;
    r.Bottom := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;

    if node = FHighlightNode then
    begin
      Canvas.Brush.Color := FHighlightColor;
      Canvas.FillRect(Rect(DrawInfo.ClientRect.Left, r.Top, DrawInfo.ClientRect.Right, r.Bottom));
    end;

    if node.ChildNodes.Count = 0 then graph := FLeafImage
    else if node.Expanded then graph := FExpandedImage
    else graph := FCollapsedImage;
    
    if graph.Width > 0 then
    begin
      graph.Draw(Self.Canvas, r.Left, r.Top + (node.Height - graph.Height) div 2);
      Inc(r.Left, FNodeTextOffset + graph.Width);
    end;

    Canvas.Font := Font;
    Windows.SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    Canvas.TextRect(r, node.FText, [tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
  end;

  if Result and node.Expanded then
  begin
    for i := 0 to node.ChildNodes.Count - 1 do
    begin
      if not Self.DrawNode(node.ChildNodes[i], DrawInfo) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TFsNavTree.GetCategoryImageRect(category: TFsNavTreeCategory; const r: TRect): TRect;
begin
  Result.Top :=  r.Top + (category.Height - category.Image.Height) div 2;
  Result.Bottom := Result.Bottom + category.Image.Height;
  Result.Left := r.Left + FCategoryImageOffset;
  Result.Right := Result.Left + category.Image.Width;
end;

function TFsNavTree.GetContentHeight: Integer;
var
  i, j: Integer;  
begin
  Result := 0;
  
  for i := 0 to FCategorys.Count - 1 do
  begin
    Inc(Result, FCategorys[i].Height);

    for j := 0 to FCategorys[i].Nodes.Count - 1 do
      Inc(Result, GetVisibleNodeTotalHeight(FCategorys[i].Nodes[j]));
  end;
end;

function TFsNavTree.GetNodeImageRect(node: TFsNavTreeNode; const r: TRect): TRect;
var
  pic: TFsPicture;
begin
  pic := GetNodePictrue(node);
  Result.Top :=  r.Top + (node.Height - pic.Height) div 2;
  Result.Bottom := Result.Bottom + pic.Height;
  Result.Left := r.Left + FCategoryImageOffset + (node.Level + 1) * FNodeIndent;
  Result.Right := Result.Left + pic.Width;
end;

function TFsNavTree.GetNodePictrue(node: TFsNavTreeNode): TFsPicture;
begin
  if node.ChildNodes.Count = 0 then Result := FLeafImage
  else if node.Expanded then Result := FExpandedImage
  else Result := FCollapsedImage;
end;

function TFsNavTree.GetVisibleNodeTotalHeight(node: TFsNavTreeNode): Integer;
var
  i: Integer;
begin
  Result := node.Height;

  if node.Expanded then
  begin
    for i := 0 to node.ChildNodes.Count - 1 do
      Inc(Result, GetVisibleNodeTotalHeight(node.ChildNodes[i]));
  end;
end;

function TFsNavTree.HitTest(X, Y: Integer; var area: TRect; var obj: TObject): TFsNavTreeHitTest;
var
  si: TScrollInfo;
  i: Integer;
  HitTestInfo: TFsNavTreeHitTestInfo;
begin
  Windows.GetClientRect(Handle, HitTestInfo.ClientRect);

  Result := htNone;

  if (X < HitTestInfo.ClientRect.Left) or (X >= HitTestInfo.ClientRect.Right)
    or (Y < HitTestInfo.ClientRect.Top) or (Y >= HitTestInfo.ClientRect.Bottom) then Exit;
  
  HitTestInfo.CurrentOffsetY := 0;

  Self.GetControlScrollInfo(si, True);

  if NeedScroll(si) then HitTestInfo.OffsetY := si.nPos
  else HitTestInfo.OffsetY := 0;

  for i := 0 to FCategorys.Count - 1 do
    if not Self.HitTestCategory(X, Y, Categorys[i], HitTestInfo) then
    begin
      Result := HitTestInfo.HitTest;
      CopyRect(area, HitTestInfo.area);
      obj := HitTestInfo.obj;
      Break;
    end;
end;

function TFsNavTree.HitTestCategory(X, Y: Integer; category: TFsNavTreeCategory; var HitTestInfo: TFsNavTreeHitTestInfo): Boolean;
var
  i, t, b: Integer;
begin
  Result := True;

  t := HitTestInfo.ClientRect.Top + HitTestInfo.CurrentOffsetY - HitTestInfo.OffsetY;
  b := t + category.Height;
  Inc(HitTestInfo.CurrentOffsetY, category.Height);

  if (Y >= t) and (Y < b) then
  begin
    HitTestInfo.area.Top := t;
    HitTestInfo.area.Bottom := b;
    HitTestInfo.area.Left := HitTestInfo.ClientRect.Left;
    HitTestInfo.area.Right := HitTestInfo.ClientRect.Right;
    HitTestInfo.obj := category;

    if PtInRect(GetCategoryImageRect(category, HitTestInfo.area), Point(X, Y)) then
      HitTestInfo.HitTest := htCategoryImage
    else
      HitTestInfo.HitTest := htCategoryText;

    Result := False;
    Exit;
  end;

  for i := 0 to category.Nodes.Count - 1 do
    if not Self.HitTestNode(X, Y, category.Nodes[i], HitTestInfo) then
    begin
      Result := False;
      Break;
    end;
end;

function TFsNavTree.HitTestNode(X, Y: Integer; node: TFsNavTreeNode; var HitTestInfo: TFsNavTreeHitTestInfo): Boolean;
var
  i, t, b: Integer;
begin
  Result := True;

  t := HitTestInfo.ClientRect.Top + HitTestInfo.CurrentOffsetY - HitTestInfo.OffsetY;
  b := t + node.Height;
  Inc(HitTestInfo.CurrentOffsetY, node.Height);

  if (Y >= t) and (Y < b) then
  begin
    HitTestInfo.area.Top := t;
    HitTestInfo.area.Bottom := b;
    HitTestInfo.area.Left := HitTestInfo.ClientRect.Left;
    HitTestInfo.area.Right := HitTestInfo.ClientRect.Right;
    HitTestInfo.obj := node;

    if PtInRect(GetNodeImageRect(node, HitTestInfo.area), Point(X, Y)) then
      HitTestInfo.HitTest := htNodeImage
    else
      HitTestInfo.HitTest := htNodeText;

    Result := False;
    Exit;
  end;

  for i := 0 to node.ChildNodes.Count - 1 do
    if not Self.HitTestNode(X, Y, node.ChildNodes[i], HitTestInfo) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TFsNavTree.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  area: TRect;
  obj: TObject;
  htr: TFsNavTreeHitTest;
begin
  inherited;

  if Button = mbLeft then
  begin
    htr := Self.HitTest(X, Y, area, obj);

    case htr of
      htNone: ;

      htCategoryImage: ;

      htCategoryText: ;

      htNodeImage:
        if TFsNavTreeNode(obj).ChildNodes.Count > 0 then
          TFsNavTreeNode(obj).Expanded := not TFsNavTreeNode(obj).Expanded;

      htNodeText:
        DoClickNode(TFsNavTreeNode(obj));
    end;
  end;
end;

procedure TFsNavTree.Paint;
var
  si: TScrollInfo;
  i: Integer;
  DrawInfo: TFsNavTreeDrawInfo;
begin
  inherited;

  DrawInfo.CurrentOffsetY := 0;
  Windows.GetClientRect(Handle, DrawInfo.ClientRect);

  Self.GetControlScrollInfo(si, True);

  if NeedScroll(si) then DrawInfo.OffsetY := si.nPos
  else DrawInfo.OffsetY := 0;

  for i := 0 to FCategorys.Count - 1 do
    if not Self.DrawCategory(FCategorys[i], DrawInfo) then Break;
end;

procedure TFsNavTree.Resize;
begin
  inherited;
  UpdateScrollRange;
end;

procedure TFsNavTree.SetCategoryFont(const Value: TFont);
begin
  FCategoryFont.Assign(Value);
  Self.Invalidate;
end;

procedure TFsNavTree.SetCategoryImageOffset(const Value: Integer);
begin
  if (Value >= 0) and (FCategoryImageOffset <> Value) then
  begin
    FCategoryImageOffset := Value;
    Self.Invalidate;
  end;
end;

procedure TFsNavTree.SetCategorys(const Value: TFsNavTreeCategorys);
begin
  if Assigned(Value) then FCategorys.Assign(Value)
  else FCategorys.Clear;
end;

procedure TFsNavTree.SetCategoryTextOffset(const Value: Integer);
begin
  if (Value >= 0) and (FCategoryTextOffset <> Value) then
  begin
    FCategoryTextOffset := Value;
    Self.Invalidate;
  end;
end;

procedure TFsNavTree.SetCollapsedImage(const Value: TFsPicture);
begin
  FCollapsedImage.Assign(Value);
  Self.Invalidate;
end;

procedure TFsNavTree.SetExpandedImage(const Value: TFsPicture);
begin
  FExpandedImage.Assign(Value);
  Self.Invalidate;
end;

procedure TFsNavTree.SetHighlightColor(const Value: TColor);
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    Self.Invalidate;
  end;
end;

procedure TFsNavTree.SetLeafImage(const Value: TFsPicture);
begin
  FLeafImage.Assign(Value);
  Self.Invalidate;
end;

procedure TFsNavTree.SetNodeIndent(const Value: Integer);
begin
  if (Value >= 0) and (FNodeIndent <> Value) then
  begin
    FNodeIndent := Value;
    Self.Invalidate;
  end;
end;

procedure TFsNavTree.SetNodeTextOffset(const Value: Integer);
begin
  if (Value >= 0) and (FNodeTextOffset <> Value) then
  begin
    FNodeTextOffset := Value;
    Self.Invalidate;
  end;
end;

procedure TFsNavTree.UpdateScrollRange;
var
  rc: TRect;
  ContentHeight: Integer;
begin
  ContentHeight := GetContentHeight;
  Windows.GetClientRect(Handle, rc);
  Self.SetScrollRange(True, 0, ContentHeight - 1, rc.Bottom - rc.Top);
end;

{ TFsNavTreeNode }

procedure TFsNavTreeNode.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TFsNavTreeNode then
    Self.FText := TFsNavTreeNode(Source).Text
  else inherited;
end;

constructor TFsNavTreeNode.Create(Collection: TCollection);
begin
  inherited;
  FHeight := 32;
  FChildNodes := TFsNavTreeNodes.Create(Self);
  FExpanded := False;
end;

destructor TFsNavTreeNode.Destroy;
begin
  FChildNodes.Free;
  inherited;
end;

function TFsNavTreeNode.GetCategory: TFsNavTreeCategory;
begin
  if Assigned(Collection) then Result := TFsNavTreeNodes(Collection).Category
  else Result := nil;
end;

function TFsNavTreeNode.GetDisplayName: string;
begin
  Result := FText;
end;

function TFsNavTreeNode.GetLevel: Integer;
var
  node: TFsNavTreeNode;
begin
  Result := 0;

  node := Self;

  while Assigned(node.Collection) and Assigned(TFsNavTreeNodes(node.Collection).Parent) do
  begin
    node := TFsNavTreeNodes(node.Collection).Parent;
    Inc(Result);
  end;   
end;

procedure TFsNavTreeNode.SetChildNodes(const Value: TFsNavTreeNodes);
begin
  FChildNodes.Assign(Value);
end;

procedure TFsNavTreeNode.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    Changed(False);
  end;
end;

procedure TFsNavTreeNode.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TFsNavTreeNode.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TFsNavTreeNodes }

constructor TFsNavTreeNodes.Create(AParent: TFsNavTreeNode);
begin
  inherited Create(TFsNavTreeNode);
  FParent := AParent;
end;

constructor TFsNavTreeNodes.Create(ACategory: TFsNavTreeCategory);
begin
  inherited Create(TFsNavTreeNode);
  FCategory := ACategory;
end;

function TFsNavTreeNodes.GetCategory: TFsNavTreeCategory;
begin
  if Assigned(FCategory) then Result := FCategory
  else if Assigned(FParent) then Result := FParent.Category
  else Result := nil;
end;

function TFsNavTreeNodes.GetItem(Index: Integer): TFsNavTreeNode;
begin
  Result := TFsNavTreeNode(inherited Items[Index]); 
end;

procedure TFsNavTreeNodes.SetItem(Index: Integer; const Value: TFsNavTreeNode);
begin
  inherited Items[Index] := Value;
end;

procedure TFsNavTreeNodes.Update(Item: TCollectionItem);
var
  lCategory: TFsNavTreeCategory;
begin
  inherited;

  lCategory := Self.Category;

  if Assigned(lCategory) then lCategory.Changed(False);
end;

{ TFsNavTreeCategory }

procedure TFsNavTreeCategory.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TFsNavTreeCategory then
  begin
    Self.FHeight := TFsNavTreeCategory(Source).Height;
    Self.FText := TFsNavTreeCategory(Source).Text;
    FNodes.Assign(TFsNavTreeCategory(Source).Nodes);
    FImage.Assign(TFsNavTreeCategory(Source).FImage);
  end
  else inherited;
end;

constructor TFsNavTreeCategory.Create(Collection: TCollection);
begin
  inherited;
  FHeight := 32;
  FImage := TFsPicture.Create;
  FImage.OnChange := Self.ImageChanged;
  FNodes := TFsNavTreeNodes.Create(Self);
end;

destructor TFsNavTreeCategory.Destroy;
begin
  FNodes.Free;
  FImage.Free;
  inherited;
end;

function TFsNavTreeCategory.GetDisplayName: string;
begin
  Result := FText;
end;

procedure TFsNavTreeCategory.ImageChanged(Sender: TObject);
begin

end;

procedure TFsNavTreeCategory.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TFsNavTreeCategory.SetImage(const Value: TFsPicture);
begin
  FImage.Assign(Value);
end;

procedure TFsNavTreeCategory.SetNodes(const Value: TFsNavTreeNodes);
begin
  FNodes.Assign(Value);
end;

procedure TFsNavTreeCategory.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TFsNavTreeCategorys }

constructor TFsNavTreeCategorys.Create(ATree: TFsNavTree);
begin
  inherited Create(TFsNavTreeCategory);
  FTree := ATree;
end;

function TFsNavTreeCategorys.GetItem(Index: Integer): TFsNavTreeCategory;
begin
  Result := TFsNavTreeCategory(inherited Items[Index]);
end;

procedure TFsNavTreeCategorys.SetItem(Index: Integer; const Value: TFsNavTreeCategory);
begin
  inherited Items[Index] := Value;
end;

procedure TFsNavTreeCategorys.Update(Item: TCollectionItem);
begin
  inherited;

  if Assigned(FTree) then
  begin
    FTree.UpdateScrollRange;
    FTree.Invalidate;
  end;
end;

end.
