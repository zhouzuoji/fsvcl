unit FSVclRegister;

interface

uses
  SysUtils, Classes, Windows, DesignEditors, DesignIntf, DesignMenus, ComponentDesigner,
  FSTheme, FSGraphics, FSControls, FSStdCtrls, FSScrollControls, FsNavTree;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FastSimpleVcl', [TFsTheme, TFsRectangle, TFsNinePitchDrawable, TFsPictureDrawable,
    TFsImage, TFsImageButton, TFsCoverButton, TFsSkinButton, TFsCheckBox,
    TFsPanel, TFsPageControl, TFsEdit, TFsButtonEdit, TFsFlatScrollBar,
    TFsMemo, TFsListBox, TFsListView, TFsTreeView, TFsNavTree]);
end;

end.
