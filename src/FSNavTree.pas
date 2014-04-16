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

  TFsNavTree = class(TFsCustomControl)
  private
    FCatalogs: TFsNavTreeCatalogs;
    procedure SetCatalogs(const Value: TFsNavTreeCatalogs);
    function GetVisibleNodeTotalHeight(node: TFsNavTreeNode): Integer;
    function GetContentHeight: Integer;
    function DrawNode(node: TFsNavTreeNode; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
    function DrawCatalog(catalog: TFsNavTreeCatalog; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateScrollRange;
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
    property Catalogs: TFsNavTreeCatalogs read FCatalogs write SetCatalogs;
  end;    

implementation

{ TFsNavTree }

constructor TFsNavTree.Create(AOwner: TComponent);
begin
  inherited;
  Width := 250;
  Height := 360;
  FCatalogs := TFsNavTreeCatalogs.Create(Self);
end;

destructor TFsNavTree.Destroy;
begin
  FCatalogs.Free;
  inherited;
end;

function TFsNavTree.DrawCatalog(catalog: TFsNavTreeCatalog; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
begin
  Result := True;

  if DrawInfo.CurrentOffsetY + catalog.Height > DrawInfo.OffsetY then

end;

function TFsNavTree.DrawNode(node: TFsNavTreeNode; var DrawInfo: TFsNavTreeDrawInfo): Boolean;
begin
  Result := True;
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
  Result := Self.Height;

  if node.Expanded then
  begin
    for i := 0 to node.ChildNodes.Count - 1 do
      Inc(Result, GetVisibleNodeTotalHeight(node.ChildNodes[i]));
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

  if NeedScroll(si) then DrawInfo.OffsetY := 0
  else DrawInfo.OffsetY := si.nPos;

  for i := 0 to FCatalogs.Count - 1 do
    if not Self.DrawCatalog(FCatalogs[i], DrawInfo) then Break;
end;

procedure TFsNavTree.SetCatalogs(const Value: TFsNavTreeCatalogs);
begin
  if Assigned(Value) then FCatalogs.Assign(Value)
  else FCatalogs.Clear;
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
