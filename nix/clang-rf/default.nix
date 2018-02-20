{ stdenv }:
assert stdenv.isDarwin;
stdenv.mkDerivation {
  name = "clang-rf";

  # Technique for cleaning up after the exec is from:
  # https://stackoverflow.com/a/24112832/277078

  # echo \''${extraBefore+"\''${extraBefore[@]}"} > "\$clang_rf_tmpfile"
# echo \''${params+"\''${params[@]}"} >> "\$clang_rf_tmpfile"
# echo \''${extraAfter+"\''${extraAfter[@]}"} >> "\$clang_rf_tmpfile"


  #     source $stdenv/setup
  buildCommand = ''
    mkdir -p $out/bin

    function wrap-rf() {
      head -n -4 ${stdenv.cc}/bin/$1 > $out/bin/rf-$1
      local exe=$(cat ${stdenv.cc}/bin/$1 | grep 'exec ' | sed 's/exec \([^[:space:]]*\).*$/\1/')
      cat << EOF >> $out/bin/rf-$1
clang_rf_tmpdir=\$(mktemp -d "\''${TMPDIR:-/tmp/}rf-$1.XXXXXXXXXXXX")
clang_rf_tmpfile="\$clang_rf_tmpdir/$1-args"
printf "%s\n" \''${extraBefore+"\''${extraBefore[@]}"} > "\$clang_rf_tmpfile"
printf "%s\n" \''${params+"\''${params[@]}"} >> "\$clang_rf_tmpfile"
printf "%s\n" \''${extraAfter+"\''${extraAfter[@]}"} >> "\$clang_rf_tmpfile"

sed 's|-B${stdenv.cc}/bin/|-B$out/bin|' -i "\$clang_rf_tmpfile"
# sed '/^-F/d' -i "\$clang_rf_tmpfile"

echo "*** CLANG-RF EXEC"
#{ while kill -0 \$$ 2>/dev/null; do sleep 5; done; rm -rf "\$clang_rf_tmpdir"; } &
trap "rm -rf \''${clang_rf_tmpdir}" EXIT
# echo "Response file: \$clang_rf_tmpfile"
unset PYTHONPATH
unset _PATH
unset CMAKE_PREFIX_PATH
unset CMAKE_LIBRARY_PATH
# unset PKG_CONFIG_PATH
# unset CMAKE_FRAMEWORK_PATH
unset ROS_PACKAGE_PATH
exec "$exe" @"\$clang_rf_tmpfile"
EOF
      sed -e 's/expandResponseParams "$@"/echo "--> Calling expandResponseParams"\nexpandResponseParams "$@"\necho "<--- expandResponseParamsComplete"/' \
          -e 's/\(path_backup="\$PATH"\)/echo "*** STARTING RF-CLANG WRAPPER"\n\1/' \
          -i $out/bin/rf-$1
      chmod a+x $out/bin/rf-$1
    }
    wrap-rf clang
    wrap-rf clang++
    ln -s $out/bin/rf-clang $out/bin/clang
    ln -s $out/bin/rf-clang++ $out/bin/clang++
    cp ${stdenv.cc}/bin/ld $out/bin/ld
    sed 's/^\(exec .*\)$/echo "^^^^^ EXECING ld"\n\1/' -i $out/bin/ld
  '';
  passAsFile = ["buildCommand"];
}
# exec "$exe" @"\$clang_rf_tmpfile"
