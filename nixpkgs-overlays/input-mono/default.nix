self: super:
{
  # note it's a new attribute and does not override old one
  input-mono = (self.input-fonts.overrideAttrs (old: {
    src = self.requireFile {
      name = "Input-Font.zip";
      url = "https://input.fontbureau.com/download/index.html?customize&fontSelection=fourStyleFamily&regular=InputMonoNarrow-Regular&italic=InputMonoNarrow-Italic&bold=InputMonoNarrow-Bold&boldItalic=InputMonoNarrow-BoldItalic&a=0&g=0&i=topserif&l=serifs_round&zero=0&asterisk=height&braces=straight&preset=default&line-height=1.2&email=";
      sha256 = "0nn41w2b6jvsbr3r4lfy4p8w2ssjmgdjzd1pbj7p0vmawjpvx2w8";
    };
    outputHash = "1w2i660dg04nyc6fc6r6sd3pw53h8dh8yx4iy6ccpii9gwjl9val";
  }));
}
