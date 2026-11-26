final: prev: {
  # nixpkgs 26.05 has one perl package set; perlPackages, which sqitch is built
  # from, is perl5Packages.
  perl5Packages = prev.perl5Packages // {
    AppSqitch = prev.perl5Packages.AppSqitch.overrideAttrs (prevAttrs: rec {
      postInstall = (prevAttrs.postInstall or "") + ''
        cp -R ${./create-migration/templates/deploy}/* $out/etc/sqitch/templates/deploy/
        cp -R ${./create-migration/templates/verify}/* $out/etc/sqitch/templates/verify/
        cp -R ${./create-migration/templates/revert}/* $out/etc/sqitch/templates/revert/
      '';
    });
  };
}
