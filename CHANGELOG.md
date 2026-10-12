# Revision history for orgmode-daedalus

## 0.1.2.0 -- 2026-10-14

* Added ability to process org-mode `#INCLUDE:` directives via the
  `OrgMode.Includes` module.  See the `insertAllInclusions` for the primary
  entrypoint for this functionality.

  Note that all included documents may result in `OrgBody` entries with an
  `isDocBody` of true even if they are in a subsection of the resulting document.

* Minor enhancements to HTML output.

## 0.1.1.0 -- 2026-07-04

* Added `includeOrgInSource` for including and parsing an org-mode file at
  compile time.  Useful for including help documents and other static documents
  in an executable while avoiding run-time failures.

* Support SUBTITLE in text-style1 output

## 0.1.0.0 -- 2026-06-03

* Initial version.
