final: prev: {
  perl540Packages = prev.perl540Packages // {
    AppSqitch = prev.perl540Packages.AppSqitch.overrideAttrs (prevAttrs: rec {
      postInstall = (prevAttrs.postInstall or "") + ''
        cp -R ${./create-migration/templates/deploy}/* $out/etc/sqitch/templates/deploy/
        cp -R ${./create-migration/templates/verify}/* $out/etc/sqitch/templates/verify/
        cp -R ${./create-migration/templates/revert}/* $out/etc/sqitch/templates/revert/
      '';
    });
  };
  perl538Packages = prev.perl538Packages // {
    AppSqitch = prev.perl538Packages.AppSqitch.overrideAttrs (prevAttrs: rec {
      postInstall = (prevAttrs.postInstall or "") + ''
        cp -R ${./create-migration/templates/deploy}/* $out/etc/sqitch/templates/deploy/
        cp -R ${./create-migration/templates/verify}/* $out/etc/sqitch/templates/verify/
        cp -R ${./create-migration/templates/revert}/* $out/etc/sqitch/templates/revert/
      '';
    });
  };
}
