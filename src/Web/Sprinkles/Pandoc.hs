module Web.Sprinkles.Pandoc
where

import Text.Pandoc

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def
  { readerStandalone =
      True
  , readerExtensions =
      disableExtension Ext_raw_html
      . disableExtension Ext_raw_attribute
      . disableExtension Ext_raw_tex
      $ pandocExtensions
  }
