unit FSNavTree;

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics, Controls, FSVclBase, FSControls;

type
  TFsNavTree = class;
  TFsNavTreeNodes = class;

  TFsNavTreeCatalog = class(TCollectionItem)
  private
    FText: string;
    FNodes: TFsNavTreeNodes;
    FImage: TPicture;
    FHeight: Integer;
    procedure SetText(const Value: string);
    procedure SetNodes(const Value: TFsNavTreeNodes);
    procedure SetImage(const Value: TPicture);
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
    property Image: TPicture read FImage write SetImage;
    property Height: Integer read FHeight write SetHeight;
  end;

  TFsNavTreeCatalogs = class(TCollection)
  private
    FTree: TFsNavTree;
    function GetItem(Index: Integer): TFsNavTreeCatalog;
    procedure SetItem(Index: Integer; const Value: TFsNavTreeCatalog);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ATree: TFsNavTree);
    property Tree: TFsNavTree read FTree;
    property Items[Index: Integer]: TFsNavTreeCatalog read GetItem write SetItem; default;
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
    function GetCatalog: TFsNavTreeCatalog;
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
    property Catalog: TFsNavTreeCatalog read GetCatalog;
    property Level: Integer read GetLevel;
  end;

  TFsNavTreeNodes = class(TCollection)
  private
    FCatalog: TFsNavTreeCatalog;
    FParent: TFsNavTreeNode;
    function GetCatalog: TFsNavTreeCatalog;
    function GetItem(Index: Integer): TFsNavTreeNode;
    procedure SetItem(Index: Integer; const Value: TFsNavTreeNode);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ACatalog: TFsNavTreeCatalog); reintroduce; overload;
    constructor Create(AParent: TFsNavTreeNode); reintroduce; overload;
    property Catalog: TFsNavTreeCatalog read GetCatalog;
    property Parent: TFsNavTreeNode read FParent;
    property Items[Index: Integer]: TFsNavTreeNode read GetItem write SetItem; default;
  end;

  TFsNavTreeDrawInfo = record
    ClientRect: TRect;
    OffsetY: Integer;
    CurrentOffsetY: Integer;
  end;
  PFsNavTreeDrawInfo = ^TFsNavTreeDrawInfo;

  TNodeClickEvent = procedure(Sender: TObject; node: TFsNavTreeNode) of object;

  TFsNavTreeHitTest = (htNone, htCatalogImage, htCatalogText, htNodeImage, htNodeText);

  TFsNavTree = class(TFsCustomControl)
  private
    FCatalogs: TFsNavTreeCatalogs;
    FCatalogImageOffset: Integer;
    FCatalogTextOffset: Integer;
    FCatalogFont: TFont;
    FLeafImage: TPicture;
    FCollapsedImage: TPicture;
    FExpandedImage: TPicture;
    FNodeIndent: Integer;
    FNodeTextOffset: Integer;
    FOnClickNode: TNodeClickEvent;
    procedure SetCatalogs(const Value: TFsNavTreeCatalogs);
    function GetVisibleNodeTotalHeight(node: TFsNavTreeNode): Integer;
    function GetContentHeight: Integer;
    function DrawNode(node: TFsNavTreeNode; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
    function DrawCatalog(catalog: TFsNavTreeCatalog; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
    procedure SetCatalogImageOffset(const Value: Integer);
    procedure SetCatalogTextOffset(const Value: Integer);
    procedure SetCatalogFont(const Value: TFont);
    procedure SetCollapsedImage(const Value: TPicture);
    procedure SetExpandedImage(const Value: TPicture);
    procedure SetLeafImage(const Value: TPicture);
    procedure SetNodeIndent(const Value: Integer);
    procedure SetNodeTextOffset(const Value: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DoCanScroll(IsVert: Boolean; var nPos: Integer); override;
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
    property Visible;
    property ParentBackground;
    property ParentCtl3D;
    property ScrollBarDrawer;
    property ScrollBars;
    property Font;
    property CollapsedImage: TPicture read FCollapsedImage write SetCollapsedImage;
    property ExpandedImage: TPicture read FExpandedImage write SetExpandedImage;
    property LeafImage: TPicture read FLeafImage write SetLeafImage;
    property NodeIndent: Integer read FNodeIndent write SetNodeIndent;
    property Catalogs: TFsNavTreeCatalogs read FCatalogs write SetCatalogs;
    property CatalogFont: TFont read FCatalogFont write SetCatalogFont;
    property CatalogImageOffset: Integer read FCatalogImageOffset write SetCatalogImageOffset;
    property CatalogTextOffset: Integer read FCatalogTextOffset write SetCatalogTextOffset;
    property NodeTextOffset: Integer read FNodeTextOffset write SetNodeTextOffset;
    property OnClickNode: TNodeClickEvent read FOnClickNode write FOnClickNode;
  end;    

implementation

{ TFsNavTree }

constructor TFsNavTree.Create(AOwner: TComponent);
begin
  inherited;
  Width := 250;
  Height := 360;
  FCatalogImageOffset := 8;
  FCatalogTextOffset := 10;
  FNodeTextOffset := 5;
  FNodeIndent := 4;
  FCatalogFont := TFont.Create;
  FCollapsedImage := TPicture.Create;
  FExpandedImage := TPicture.Create;
  FLeafImage := TPicture.Create;
  FCatalogs := TFsNavTreeCatalogs.Create(Self);
end;

destructor TFsNavTree.Destroy;
begin
  FCatalogs.Free;
  FCatalogFont.Free;
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

function TFsNavTree.DrawCatalog(catalog: TFsNavTreeCatalog; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
var
  r: TRect;
  i: Integer;
begin
  r.Top := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;
  
  Inc(DrawInfo.CurrentOffsetY, catalog.Height);

  Result := DrawInfo.CurrentOffsetY < DrawInfo.OffsetY + DrawInfo.ClientRect.Bottom - DrawInfo.ClientRect.Top;

  if DrawInfo.CurrentOffsetY > DrawInfo.OffsetY then
  begin
    r.Left := DrawInfo.ClientRect.Left + FCatalogImageOffset;
    r.Right := DrawInfo.ClientRect.Right;
    r.Bottom := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;

    if Assigned(catalog.Image.Graphic) then
    begin
      Canvas.Draw(r.Left, r.Top + (catalog.Height - catalog.Image.Height) div 2, catalog.Image.Graphic);
      Inc(r.Left, FCatalogTextOffset + catalog.Image.Width);
    end;

    Canvas.Font := FCatalogFont;
    Windows.SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    Canvas.TextRect(r, catalog.FText, [tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
  end;

  if Result then
  begin
    for i := 0 to catalog.Nodes.Count - 1 do
    begin
      if not Self.DrawNode(catalog.Nodes[i], DrawInfo) then
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
  graph: TGraphic;
begin
  r.Top := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;
  
  Inc(DrawInfo.CurrentOffsetY, node.Height);

  Result := DrawInfo.CurrentOffsetY < DrawInfo.OffsetY + DrawInfo.ClientRect.Bottom - DrawInfo.ClientRect.Top;

  if DrawInfo.CurrentOffsetY > DrawInfo.OffsetY then
  begin
    r.Left := DrawInfo.ClientRect.Left + FCatalogImageOffset + (node.Level + 1) * FNodeIndent;
    r.Right := DrawInfo.ClientRect.Right;
    r.Bottom := DrawInfo.ClientRect.Top + DrawInfo.CurrentOffsetY - DrawInfo.OffsetY;

    if node.ChildNodes.Count = 0 then graph := FLeafImage.Graphic
    else if node.Expanded then graph := FExpandedImage.Graphic
    else graph := FCollapsedImage.Graphic;
    
    if Assigned(graph) and not graph.Empty then
    begin
      Canvas.Draw(r.Left, r.Top + (node.Height - graph.Height) div 2, graph);
      Inc(r.Left, FNodeTextOffset + graph.Width);
    end;

    Canvas.Font := Font;
    Windows.SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    Canvas.TextRect(r, node.FText, [tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
  end;

  if Result then
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

function TFsNavTree.GetContentHeight: Integer;
var
  i, j: Integer;  
begin
  Result := 0;
  
  for i := 0 to FCatalogs.Count - 1 do
  begin
    Inc(Result, FCatalogs[i].Height);

    for j := 0 to FCatalogs[i].Nodes.Count - 1 do
      Inc(Result, GetVisibleNodeTotalHeight(FCatalogs[i].Nodes[j]));
  end;
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
begin

end;

procedure TFsNavTree.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
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

  for i := 0 to FCatalogs.Count - 1 do
    if not Self.DrawCatalog(FCatalogs[i], DrawInfo) then Break;
end;

procedure TFsNavTree.SetCatalogFont(const Value: TFont);
begin
  FCatalogFont.Assign(Value);
  Self.Invalidate;
end;

procedure TFsNavTree.SetCatalogImageOffset(const Value: Integer);
begin
  if (Value >= 0) and (FCatalogImageOffset <> Value) then
  begin
    FCatalogImageOffset := Value;
    Self.Invalidate;
  end;
end;

procedure TFsNavTree.SetCatalogs(const Value: TFsNavTreeCatalogs);
begin
  if Assigned(Value) then FCatalogs.Assign(Value)
  else FCatalogs.Clear;
end;

procedure TFsNavTree.SetCatalogTextOffset(const Value: Integer);
begin
  if (Value >= 0) and (FCatalogTextOffset <> Value) then
  begin
    FCatalogTextOffset := Value;
    Self.Invalidate;
  end;
end;

procedure TFsNavTree.SetCollapsedImage(const Value: TPicture);
begin
  FCollapsedImage.Assign(Value);
  Self.Invalidate;
end;

procedure TFsNavTree.SetExpandedImage(const Value: TPicture);
begin
  FExpandedImage.Assign(Value);
  Self.Invalidate;
end;

procedure TFsNavTree.SetLeafImage(const Value: TPicture);
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

function TFsNavTreeNode.GetCatalog: TFsNavTreeCatalog;
begin
  if Assigned(Collection) then Result := TFsNavTreeNodes(Collection).Catalog
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

constructor TFsNavTreeNodes.Create(ACatalog: TFsNavTreeCatalog);
begin
  inherited Create(TFsNavTreeNode);
  FCatalog := ACatalog;
end;

function TFsNavTreeNodes.GetCatalog: TFsNavTreeCatalog;
begin
  if Assigned(FCatalog) then Result := FCatalog
  else if Assigned(FParent) then Result := FParent.Catalog
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
  lCatalog: TFsNavTreeCatalog;
begin
  inherited;

  lCatalog := Self.Catalog;

  if Assigned(lCatalog) then lCatalog.Changed(False);
end;

{ TFsNavTreeCatalog }

procedure TFsNavTreeCatalog.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TFsNavTreeCatalog then
  begin
    Self.FHeight := TFsNavTreeCatalog(Source).Height;
    Self.FText := TFsNavTreeCatalog(Source).Text;
    FNodes.Assign(TFsNavTreeCatalog(Source).Nodes);
    FImage.Assign(TFsNavTreeCatalog(Source).FImage);
  end
  else inherited;
end;

constructor TFsNavTreeCatalog.Create(Collection: TCollection);
begin
  inherited;
  FHeight := 32;
  FImage := TPicture.Create;
  FImage.OnChange := Self.ImageChanged;
  FNodes := TFsNavTreeNodes.Create(Self);
end;

destructor TFsNavTreeCatalog.Destroy;
begin
  FNodes.Free;
  FImage.Free;
  inherited;
end;

function TFsNavTreeCatalog.GetDisplayName: string;
begin
  Result := FText;
end;

procedure TFsNavTreeCatalog.ImageChanged(Sender: TObject);
begin

end;

procedure TFsNavTreeCatalog.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TFsNavTreeCatalog.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

procedure TFsNavTreeCatalog.SetNodes(const Value: TFsNavTreeNodes);
begin
  FNodes.Assign(Value);
end;

procedure TFsNavTreeCatalog.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TFsNavTreeCatalogs }

constructor TFsNavTreeCatalogs.Create(ATree: TFsNavTree);
begin
  inherited Create(TFsNavTreeCatalog);
  FTree := ATree;
end;

function TFsNavTreeCatalogs.GetItem(Index: Integer): TFsNavTreeCatalog;
begin
  Result := TFsNavTreeCatalog(inherited Items[Index]);
end;

procedure TFsNavTreeCatalogs.SetItem(Index: Integer; const Value: TFsNavTreeCatalog);
begin
  inherited Items[Index] := Value;
end;

procedure TFsNavTreeCatalogs.Update(Item: TCollectionItem);
begin
  inherited;

  if Assigned(FTree) then
  begin
    FTree.UpdateScrollRange;
    FTree.Invalidate;
  end;
end;

end.
