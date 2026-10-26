# Contributing

Thank you for considering a contribution to Compare Groundwater Levels.

## Before contributing

- Read and follow [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).
- Do not include personal information, credentials, confidential material or non-public government data.
- Search existing issues and pull requests before starting duplicate work.
- For a substantial change, open an issue first to discuss the proposed approach.

## Development setup

1. Fork or clone the repository.
2. Create a focused branch:

```bash
git checkout -b feature/short-description
```

3. Install the required R packages:

```r
install.packages(c(
  "shiny",
  "readr",
  "ggplot2",
  "patchwork",
  "dplyr",
  "lubridate"
))
```

4. Run the application:

```r
shiny::runApp()
```

## Accessibility expectations

Changes must not reduce accessibility. Test affected workflows for:

- keyboard-only operation;
- visible keyboard focus;
- logical headings and landmarks;
- explicit labels and instructions;
- accessible validation and error messages;
- adequate colour contrast;
- usability at 200% and 400% zoom;
- non-colour ways of distinguishing plotted information; and
- meaningful text alternatives or summaries for important visual output.

## Testing changes

Before submitting a pull request:

1. Start the app in a clean R session.
2. Test two valid well numbers over a date range that contains data.
3. Test invalid well numbers, duplicate wells and a reversed date range.
4. Test a range with no observations.
5. Test the same-depth-range option.
6. Review the R console for warnings and errors.
7. Navigate all controls with the keyboard.
8. Check the layout at narrow viewport widths and increased browser zoom.

Include automated tests with `testthat` or `shinytest2` when adding logic that can be reliably tested.

## Pull requests

A pull request should:

- describe the problem and proposed solution;
- link the relevant issue, if one exists;
- list the tests performed;
- identify accessibility impacts;
- include screenshots for material interface changes; and
- contain only changes relevant to the stated purpose.

Repository maintainers may request revisions before merging. Approval and merging remain subject to Province of British Columbia repository governance and publication requirements.

## Reporting security or privacy concerns

Do not disclose a suspected vulnerability, credential or privacy incident in a public issue. Use the applicable Government of British Columbia internal security or privacy reporting process and notify the repository maintainer through an approved internal channel.

## Licence for contributions

By submitting a contribution, you agree that it will be licensed under the repository's [Apache License 2.0](LICENSE.md).
