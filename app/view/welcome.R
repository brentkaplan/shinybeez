box::use(
  readr,
  shiny,
)

#' @export
sidebar_ui <- function(id) {
  shiny$includeHTML("app/static/html/welcome_sidebar.html")
}

#' @export
body_ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tags$html(
    shiny$tags$body(
      ## about section
      shiny$tags$div(
        id = "about",
        shiny$HTML(
          paste0(
          "
          <h3>About <em>shinybeez</em></h3>
          <em>shinybeez</em> is a a free and open-source web application
          designed to streamline behavioral economic analyses of demand and
          discounting data. <em>shinybeez</em> provides an intuitive interface for
          conducting descriptive and inferential analyses without requiring
          programming expertise. The app integrates features previously
          scattered across multiple tools, allowing you to upload data,
          calculate empirical measures, identify systematic datasets, fit
          nonlinear models, and visualize results - all within a single
          platform. <em>shinybeez</em> supports various analysis types for demand
          data and can score multiple discounting tasks, including
          the 27-Item Monetary Choice Questionnaire.
          <br>
          "
          )
        )
      ),
      shiny$tags$div(
        id = "citation",
        shiny$HTML(
          paste0(
          "
          <h3>Citing <em>shinybeez</em></h3>
          If you use any part of <em>shinybeez</em> in your work, please cite it:
          <br>
          <br>
          Kaplan, B. A. & Reed. D. D. (under review). <em>Shinybeez: A Shiny app
          for behavioral economic easy demand and discounting.</em>
          <br>
          "
          )
        )
      ),
      shiny$tags$div(
        id = "contact",
        shiny$HTML(
          paste0(
          "
          <h3>Contact and Issues</h3>
          If you have any questions, comments, or suggestions, please contact
          Brent Kaplan at <a href='mailto:bkaplan.ku@gmail.com'>
          bkaplan.ku@gmail.com</a>. If you encounter any bugs or
          issues, please report them on the Github respository's
          <a href='https://github.com/brentkaplan/shinybeez/issues'> issues</a>
          page.
          <br>
          "
          )
        )
      ),
      shiny$tags$div(
        id = "templates",
        shiny$HTML(
          paste0(
            "
            <h3>Downloadable Templates</h3>
            <br>
            <h4>Demand templates</h4>
            <h5>Wide Format</h5>
            Data collected from, say, Qualtrics, tend to come in what is
            called 'wide' format
            (see <a href = 'https://vita.had.co.nz/papers/tidy-data.pdf'>Hadley Wickham's paper</a> on tidy data
            for more on the 'wide' and 'long' data). 'Wide' format data typically
            consists of one row per participant and each price (or whatever the independent
            variable is) is a column.
            By default, <em>shinybeez</em> takes in this style of data.
            <br>
            <figure style='margin-top: 10px;'>
              <img style='width: 100%; max-width: 800px;' src='static/img/demand-template-wide-example.png' alt='Example demand data'>
              <figcaption><em>Example demand data formatted in the wide template.</em></figcaption>
            </figure>
            "
          )
        ),
        shiny$downloadButton(
          ns("download_demand_wide"),
          style = "margin-bottom: 10px;",
          "Demand template (wide format)"
        ),
        shiny$HTML(
          paste0(
            "
            <br>
            <br style = 'display: block; content: ''; margin-top: 25px;'>
            <h5>Long Format</h5>
            You may also provide data in 'long' format. In order to specify
            this non-default format, open in the 'Specs' panel on the sidebar
            of the 'Demand' tab and select 'Long' under 'Data in long or
            wide format?' You can download a template for long format here:
            <br>
            <figure style='margin-top: 10px;'>
              <img style='width: 100%; max-width: 400px; height: 500px;' src='static/img/demand-template-long-example.png' alt='Example demand data'>
              <figcaption><em>Example demand data formatted in the long template.</em></figcaption>
            </figure>
            "
          )
        ),
        shiny$downloadButton(
          ns("download_demand_long"),
          style = "margin-bottom: 10px;",
          "Demand template (long format)"
        ),
        shiny$HTML(
          paste0(
            "
            <br>
            <br style = 'display: block; content: ''; margin-top: 25px;'>
            <h5>Wide Format (one grouping variable)</h5>
            <em>shinybeez</em> also has the ability (<em>currently</em>) to accept
            one grouping argument. In order to specify this behavior, open
            the 'Specs' panel on the sidebar of the 'Demand' tab and select
            the box next to 'Do you have a grouping variable?'
            <br>
            <br>
            When you upload your data, make sure to your grouping column is
            called 'group' in the data. Future releases of <em>shinybeez</em> will
            allow you to specify the name of your grouping column.
            <br>
            <figure style='margin-top: 10px;'>
              <img style='width: 100%; max-width: 800px;' src='static/img/demand-template-wide-onegroup-example.png' alt='Example demand data'>
              <figcaption><em>Example demand data with one grouping variable formatted in the wide template.</em></figcaption>
            </figure>
            "
          )
        ),
        shiny$downloadButton(
          ns("download_demand_wide_group"),
          style = "margin-bottom: 10px;",
          "Demand template (wide format, one grouping column)"
        ),
        shiny$HTML(
          paste0(
            "
            <br>
            <br style = 'display: block; content: ''; margin-top: 25px;'>
            <h5>Long Format (one grouping variable)</h5>
            <em>shinybeez</em> also has the ability (<em>currently</em>) to accept
            one grouping argument. In order to specify this behavior, open
            the 'Specs' panel on the sidebar of the 'Demand' tab and select
            the box next to 'Do you have a grouping variable?'
            <br>
            <br>
            When you upload your data, make sure to your grouping column is
            called 'group' in the data. Future releases of <em>shinybeez</em> will
            allow you to specify the name of your grouping column.
            <figure style='margin-top: 10px;'>
              <img style='width: 100%; max-width: 420px; height: 500px;' src='static/img/demand-template-long-onegroup-example.png' alt='Example demand data'>
              <figcaption><em>Example demand data with one grouping variable formatted in the long template.</em></figcaption>
            </figure>
            "
          )
        ),
        shiny$downloadButton(
          ns("download_demand_long_group"),
          style = "margin-bottom: 10px;",
          "Demand template (long format, one grouping column)"
        ),
        shiny$HTML(
          paste0(
            "
            <br>
            <br>
            <h4>Discounting templates</h4>
            <h5>Wide Format - 27-Item Monetary Choice Questionnaire</h5>
            <em>shinybeez</em> allows you to analyze data from the 27-Item
            Monetary Choice Questionnaire in wide format.
            The first column contains the subject
            id and each subsequent column contains a response (0 for SS or
            1 for LL).
            <br>
            "
          )
        ),
        shiny$downloadButton(
          ns("download_27mcq_wide"),
          style = "margin-top: 10px; margin-bottom: 10px;",
          "27-Item MCQ template (wide format)"
        ),
        shiny$HTML(
          paste0(
            "
            <br>
            <br>
            <h5>Long Format - 27-Item Monetary Choice Questionnaire</h5>
            <em>shinybeez</em> allows you to analyze data from the 27-Item
            Monetary Choice Questionnaire when provided in long format.
            The first column contains the subject
            id, the second column contains the question id, and the third
            column contains the response.
            <br>
            "
          )
        ),
        shiny$downloadButton(
          ns("download_27mcq_long"),
          style = "margin-top: 10px; margin-bottom: 10px;",
          "27-Item MCQ template (long format)"
        )
      ),
      ## demand section
      shiny$tags$div(
        id = "overview-demand",
        shiny$HTML(
          paste0(
            "
            <br>
            <h2>Demand</h2>
            <br>
            <h3>Overview</h3>
            Upon clicking the demand tab, you will see a collapsable sidebar where
            a data file is selected for upload, as well as additional specifications
            related to analyzing the data. Currently, the available options to select
            are whether a grouping variable should be integrated into the results,
            whether Q<sub>0</sub> should be constrained to a specific value, select whether
            to use the exponentiated
            (<a href='https://doi.org/10.1037/pha0000045'>Koffarnus et al, 2015</a>) or
            exponential
            (<a href='https://doi.org/10.1037/0033-295X.115.1.186'>Hursh & Silberberg, 2008</a>)
            demand equation, select
            what k value method should be used, and the analysis type (discussed
            below). In the main panel of the app are two boxes with embedded tabs.
            The box at the top contains information related to the data (i.e.,
            information that does not require any curve fitting) and the box at the
            bottom contains information related to the results of the curve fitting.
            Once data are uploaded, the top box is populated with information
            (by default, the uploaded data is displayed), and results
            in the bottom box are populated after you click the 'Calculate' button.

            An important feature to note is that each of the tables displayed in the
            demand page can be exported in the following ways: 'Copy' will copy the
            contents of the table to the clipboard; 'Print' will open an option to print
            the table through the device print interface; 'CSV' will prompt saving
            the table as a comma-separated text file; 'Excel' will prompt saving the table
            as a Microsoft Excel file with a .xslx extension; 'PDF' will prompt to download
            the table as a file with a .pdf extention.
            "
          )
        )
      ),
      shiny$tags$div(
        id = "systematic-demand",
        shiny$HTML(
          paste0(
            "
            <h3>Systematic Criteria</h3>
            The last tab contains a table of criteria used for <em>identifying</em>
            systematic datasets according to the
            <a href='https://doi.org/10.1037/pha0000020'>Stein et al. (2015)</a> algorithm.
            The reported values include the number of total criteria passed and whether
            each individual criterion was met (passed) or not (fail). For each criterion,
            the value is provided. Finally, the number of total positive values is
            included in the final column. Note that this tab has additional features.
            When you click to open the collapsable sidebar <em>within this tab</em>,
            you will be
            presented with adjustments that can be made to the criterion values
            outlined in <a href='https://doi.org/10.1037/pha0000020'>Stein et al. (2015)</a>.
            These values
            are set at the defaults outlined in
            <a href='https://doi.org/10.1037/pha0000020'>Stein et al. (2015)</a>
            and changing these values will update the table. When a grouping variable is
            specified, a group identificatio ncolumn is added, but the results do not change
            as these criteria are only applied at the individual level.
            "
          )
        )
      ),
      shiny$tags$div(
        id = "regression-demand",
        shiny$HTML(
          paste0(
            "
            <h3>Nonlinear Regression</h3>
            The bottom box on the demand page has two tabs and displays the results of
            the nonlinear curve fitting process in a tabular format (Model Results tab) and
            graphical format (Plots tab). In the Model Results tab, a table is displayed
            with the following information: equation, estimated Q<sub>0</sub> (including
            standard error and confidence intervals), k value, estimated
            alpha (including standard error and confidence intervals), R<sup>2</sup>, model
            absolute sum of squares, standard deviation of the residuals, essential value,
            derived O<sub>max</sub>, derived P<sub>max</sub>, and
            analytic O<sub>max</sub> and analytic P<sub>max</sub>
            (see <a href='https://doi.org/10.1037/pha0000268'>
            Gilroy, 2019</a>). The last column of the table provides
            a quick note on whether the model converged, and the results for a model that
            has indication other than convergence should be interpreted with caution.
            <br>
            <br>
            The aforementioned model results are applied to the data depending on the
            analysis type chosen in the sidebar of the demand page. When the Fit to Group
            (pooled) analysis type is chosen, a single regression model is fit to the
            entire sample (all data points), and any dependence or clustering is ignored.
            When the Fit to Group (mean) analysis
            type is chosen, data are first aggregated by taking the mean of the y values at
            each x value and a regression line is fit to those points. When the
            Two Stage analysis type is chosen, a regression
            model is fit to each individual subject separately. When a grouping variable
            is specified the analysis types Fit to Group (pooled) and Fit to Group (mean)
            will fit curves as just described except <em>within</em> each group. There is no
            effect on whether a grouping variable is specified when the Two Stage analysis
            type is selected.
            <br>
            <br>
            The second tab in the results box contains a plot of the results based
            on the settings specified in the sidebar, especially Analysis Type. Before
            describing and showing what these plots look like, we describe some
            plot customization settings in the collapsible side bar in the plot tab.
            You may customize the plot title, the x-axis title, the y-axis title, and
            (when applicable) the group legend title. In addition, you can specify
            whether to (pseudo)log transform either the x or y axes.
            Then, you may output the plot directly as a png, svg, or jpeg file, or
            display the plot in the viewer window and choose additional formats such
            as pdf, bmp, eps, or tiff. In this plot viewer, you may specify specific
            height and width values, as well as file name.
            <br>
            <br>
            When Fit to Group (pooled) analysis type is selected, all data points
            will display with the best-fit line. Similarly, when
            a group is specified the points and lines will be colored according to the
            groups. When Fit to Group (mean) analysis type is specified,
            the preprocessed average data points are displayed with the best fit line
             and when a group is specified, the points and lines will be
            colored according to the groups. Finally, when
            Two Stage analysis type is selected, plots will be displayed in one of two
            ways. When there are 50 or fewer IDs (e.g., respondents), each ID will get
            their own subplot and when there are more than 50 IDs all
            curves and points will be shown on a single plot.
            "
          )
        )
      ),
      shiny$tags$div(
        id = "cites-demand",
        shiny$HTML(
          paste0(
            "
            <h3>Suggested Citations</h3>
            <br>
            <b>Underyling R package driving demand functions:</b>
            <br>
            <br>
            <a href='https://1drv.ms/b/s!AlLnuAdcvsULgWVDp1aUATS0rafj'>
            <i class='fa-regular fa-file-pdf'></i></a>
            Kaplan, B. A., Gilroy, S. P., Reed, D. D., Koffarnus, M. N., &
            Hursh, S. R. (2019). The R package <em>beezdemand</em>: Behavioral Economic
            Easy Demand. <em>Perspectives on Behavior Science,
            42</em>(1), 163-180. <a href='https://doi.org/10.1007/s40614-018-00187-7'>
            https://doi.org/10.1007/s40614-018-00187-7</a>

            <br>
            <br>
            <b>Systematic criteria:</b>
            <br>
            <br>
            Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J.,
            & Bickel, W. K. (2015). Identification and management of
            nonsystematic purchase task data: Toward best practice.
            <em>Experimental and Clinical Psychopharmacology, 23</em>(5),
            377-386. <a href='https://doi.org/10.1037/pha0000020'>
            https://doi.org/10.1037/pha0000020</a>
            <br>
            <br>
            <b>Exponentiated demand equation:</b>
            <br>
            <br>
            Koffarnus, M. N., Franck, C. T., Stein, J. S., & Bickel, W. K.
            (2015). A modified exponential behavioral economic demand model
            to better describe consumption data. <em>Experimental and Clinical
            Psychopharmacology, 23</em>(6), 504-512.
            <a href='https://doi.org/10.1037/pha0000045'>
            https://doi.org/10.1037/pha0000045</a>
            <br>
            <br>
            <b>Exponential demand equation:</b>
            <br>
            <br>
            Hursh, S. R., & Silberberg, A. (2008). Economic demand and
            essential value. <em>Psychological Review, 115</em>(1),
            186-198. <a href='https://doi.org/10.1037/0033-295X.115.1.186'>
            https://doi.org/10.1037/0033-295X.115.1.186</a>
            <br>
            <br>
            "
          )
        )
      ),
      shiny$tags$div(
        id = "faq-demand",
        shiny$HTML(
          paste0(
          "
          <h3>Frequently Asked Questions</h3>
          <br>
          <h5>Why is the F-test for group demand curves not available?</h5>
          <p>Over time, with the increased awareness of mixed effects models,
          we believe that using the 'Extra Sum of Squares F-test' is probably
          not a good idea to test for differences in group-level demand curves
          when the demand curves are fit using fixed effects only (i.e., no
          random effects). The issue boils down to the purpose of group-level
          only modeling. We believe that while this approach may be beneficial
          to ascertain an overall trend in the data and to <em>describe</em>
          the data, you should not use the results for subsequent
          <em>inferential</em> tests, such as the F-test. Whether you use the
          pooled approach or the fit to means approach, both of these
          techniques disregard the clustering and interdependency of points
          within an individual. For a lengthier discussion, please see
          <a href='https://1drv.ms/b/s!AlLnuAdcvsULgiBH5AxxQdF9Bp3y'>
            Kaplan et al. (2021)</a>.
          </p>
          <h5>Where are the other equations?</h5>
          <p>Currenty <em>shinybeez</em> supports two of the most popular demand
          models. This limitation is in part due to the <em>beezemand</em>
          R package currently supporting only those two models. We expect
          additional models to be implemented in the near future.</p>
          <h5>How can I conduct cross-price elasticity analyses?</h5>
          <p>This is currently not supported in <em>shinybeez</em>. However,
          we are working on it and plan to have that feature in a future
          release</p>
          "
          )
        ),
        ),
      ## discounting section
      shiny$tags$div(
        id = "overview-discounting",
        shiny$HTML(
          paste0(
            "
            <br>
            <h2>Discounting</h2>
              <br>
            <h3>Overview</h3>
            <p>
            <em>shinybeez</em> allows you to analyze data from the
            27-Item Monetary Choice Questionnaire and from the
            minute discounting tasks (as administered via the Qualtrics
            template).
            </p>
            "
          )
        )
      ),
      shiny$tags$div(
        id = "mcq",
        shiny$HTML(
          paste0(
            "
            <h3>27-Item Monetary Choice Questionnaire</h3>
            <p>
            The analyses and outputs for scoring the 27-Item MCQ are nearly
            identical to that of the 21- and 27-Item Monetary Choice
            Questionnaire Automated Scorer developed in Microsoft Excel (<a
            href='https://doi.org/10.1007/s40614-016-0070-9'>Kaplan et al.,
            2016</a>; <a
            href='https://kuscholarworks.ku.edu/handle/1808/15424'>Kaplan et
            al., 2014</a>). When you upload your spreadsheet in the specified
            format, two additional settings will appear in the sidebar. The
            first is a selector for imputing missing values (described next),
            and the second is a selector for transforming resulting k values
            by: 'none' (no transformation), 'log' (log base 10), and 'ln' (log
            base e or natural log). These selectors will have a result on the
            output in the bottom box of the Discounting page but will have no
            effect on the top box of the discounting page that displays the
            uploaded data and the proportion of missing values for each
            subject ID.

            <br>
            <h6>Treatment of missing values</h6>
            Missing values can be dealt with according to the methods outlined by
            <a href='https://doi.org/10.1371/journal.pone.0292258'>
            Yeh et al. (2023)</a>: 'none' (no imputation); 'GGM' (group
            geometric mean); 'INN (no random)' (item nearest neighbor); and
            'INN (random)'
            (item nearest neighbor with a random component). We will briefly
            discuss these
            methods, but direct readers to
            <a href='https://doi.org/10.1371/journal.pone.0292258'>
            Yeh et al. (2023)</a> for a full,
            comprehensive treatment of the approaches. After calculating the
            small, medium, and large k values for each person, the 'GGM'
            option calculates the geometric (i.e., composite) k value of the
            three amount sets <em>so long as one of the amount sets has been
            calculated</em>. For the 'INN (no random), the missing value will
            be imputed as the value from the adjacent small, medium, or large
            response(s) so long as the non-missing responses are the same. For
            the 'INN (random)', the same procedure is used except in the case
            of non-identical responses, the missing response will be replaced
            by either a 0 (i.e., SIR/SS) or 1 (i.e., LDR/LL) chosen from a
            binomial distribution with equal probabilities. Given the
            randomness of this latter approach, a table of the provided data
            along with an additional column with the new, imputed response is
            provided in the bottom box of the Discounting page. For a more
            complete description and comparisons of these methods, see the
            paper by <a href='https://doi.org/10.1371/journal.pone.0292258'>
            Yeh et al. (2023)</a>.
            </p>
            "
          )
        )
      ),
      shiny$tags$div(
        id = "5trial",
        shiny$HTML(
          paste0(
            "
            <h3>5-trial discounting task</h3>
            <p>
            <em>shinybeez</em> also has the ability to automatically score results from
            the delay and probability minute discounting tasks
            <em>provided in the Qualtrics Minute Discounting Task template</em>
            (see
            <a href='https://doi.org/10.13140/RG.2.2.26495.79527'>Koffarnus et al., 2017</a>;
            <a href='https://doi.org/10.13140/RG.2.2.31281.92000'>Koffarnus et al., 2021</a>).
            The Results
            box displays an exportable table of the results including: ResponseId;
            question index; ordered question number; page timing metadata;
            response (SS or LL) for each question index; whether the respondent's
            responses were flagged by the attention question (see
            <a href='https://doi.org/10.13140/RG.2.2.31281.92000'>Koffarnus et al., 2021</a>
            for additional information about this specific task); k value; and Effective
            Delay 50 value (see
            <a href='https://doi.org/10.1016/j.drugalcdep.2007.12.011'>Yoon & Higgins, 2008</a>).
            In the case of the probability
            minute task, the h value (instead of the k value), Effective &Theta;50 value
            (instead of Effective Delay 50 value), and Effective Probability 50 value are
            displayed.
            </p>
            "
          )
        )
      ),
      shiny$tags$div(
        id = "cites-discounting",
        shiny$HTML(
          paste0(
            "
            <h3>Suggested Citations</h3>
            <br>
            <b>Underyling R package driving discounting functions:</b>
            <br>
            <br>
            Kaplan, B. A. (2023). <em>beezdiscounting</em>: Behavioral
            Economic Easy Discounting. R package version 0.3.1,
            <a href='https://CRAN.R-project.org/package=beezdiscounting'>
            https://CRAN.R-project.org/package=beezdiscounting</a>
            <br>
            <br>
            <b>Describing scoring of the 21- and 27-Item Monetary Choice Questionnaires:</b>
            <br>
            <br>
            <a href='https://1drv.ms/b/s!AlLnuAdcvsULgWNk-mTus1oAJkyD'>
            <i class='fa-regular fa-file-pdf'></i></a>
            Kaplan, B. A., Amlung, M., Reed, D. D., Jarmolowicz, D. P.,
            McKerchar, T. L., & Lemley, S. M. (2016). Automating scoring of
            delay discounting for the 21- and 27-Item Monetary Choice
            Questionnaires. <em>The Behavior Analyst, 39</em>(2),
            293-304. <a href='https://doi.org/10.1007/s40614-016-0070-9'>
            https://doi.org/10.1007/s40614-016-0070-9</a>
            <br>
            <br>
            <b>Seminal article introducing the 27-Item Monetary Choice Questionnaire:</b>
            <br>
            <br>
            Kirby, K. N., Petry, N. M., & Bickel, W. K. (1999).
            Heroin addicts have higher discount rates for delayed rewards
            than non-drug-using controls. <em>Journal of Experimental
            Psychology: General, 128</em>(1), 78-87.
            <a href='https://doi.org/10.1037//0096-3445.128.1.78'>
            https://doi.org/10.1037//0096-3445.128.1.78</a>
            <br>
            <br>
            <b>Imputing missing data in the 27-Item Monetary Choice Questionnaire:</b>
            <br>
            <br>
            Yeh, Y.-H., Tegge, A. N., Freitas-Lemos, R., Myerson, J., Green, L.,
            & Bickel, W. K. (2023). Discounting of delayed rewards: Missing
            data imputation for the 21- and 27-item monetary choice
            questionnaires. <em>PLOS ONE, 18</em>(10),
            e0292258. <a href='https://doi.org/10.1371/journal.pone.0292258'>
            https://doi.org/10.1371/journal.pone.0292258</a>
            <br>
            <br>
            <b>Minute discounting task:</b>
            <br>
            <br>
            Koffarnus, M. N., & Bickel, W. K. (2014). A 5-trial adjusting
            delay discounting task: Accurate discount rates in less than 60
            seconds. <em>Experimental and Clinical Psychopharmacology,
            22</em>(3), 222-228. <a href='https://doi.org/10.1037/a0035973'>
            https://doi.org/10.1037/a0035973</a>
            <br>
            <br>
            Koffarnus, M. N., Rzeszutek, M. J., & Kaplan, B. A. (2021).
            Additional discounting rates in less than one minute: Task variants
            for probability and a wider range of delays. <a href='https://doi.org/10.13140/RG.2.2.31281.92000'>
            https://doi.org/10.13140/RG.2.2.31281.92000</a>
            <br>
            <br>
            Koffarnus, M. N., Kaplan, B. A., & Stein, J. S. (2017). User guide
            for Qualtrics minute discounting template.
            <a href='https://doi.org/10.13140/RG.2.2.26495.79527'>
            https://doi.org/10.13140/RG.2.2.26495.79527</a>
            <br>
            "
          )
        )
      ),
      shiny$tags$div(
        id = "faq-discounting",
        shiny$HTML(
          paste0(
            "
            <h3>Frequently Asked Questions</h3>
            <br>
            <h5>Why does the Kirby et al table say .025 but the results of
            <em>shinybeez</em> say something different?</h5>
            <p>This is because the original paper by Kirby and colleagues
            presented rounded k values. <em>shinybeez</em> and the underlying
            R package <em>beezdiscounting</em>, as well as the 21- and 27-Item
            Monetary Choice Questionnaire Automated Scorers do not provide
            the rounded values. For more information about how those k values
            are calculated, please see the article by
            <a href='https://1drv.ms/b/s!AlLnuAdcvsULgWNk-mTus1oAJkyD'>
            Kaplan et al. (2016)</a> where they discuss derivation of k
            values in the context of the 27-Item MCQ.
             </p>
            <h5>How can I calculate Area Under the Curve or
            conduct nonlinear regression on discounting?</h5>
            <p>We are rapidly working on adding these functionalities to
            <em>shinybeez</em>, as well as the underlying R package
            <em>beezdiscounting</em>. Please check back soon for new features!
             </p>
            "
          )
        )
      ),
      ## references resources section
      shiny$tags$div(
        id = "resources",
        shiny$HTML(
          paste0(
            "
            <h2>Helpful Resources</h2>
            <br>
            <b>Applying mixed effects models for behavioral economic demand:</b>
            <br>
            <br>
            <a href='https://1drv.ms/b/s!AlLnuAdcvsULgiBH5AxxQdF9Bp3y'>
            <i class='fa-regular fa-file-pdf'></i></a>
            Kaplan, B. A., Franck, C. T., McKee, K., Gilroy, S. P., &
            Koffarnus, M. N. (2021). Applying mixed-effects modeling to
            behavioral economic demand: An introduction. <em>Perspectives on
            Behavior Science, 44</em>(2), 333-358. <a href='https://doi.org/10.1007/s40614-021-00299-7'>
            https://doi.org/10.1007/s40614-021-00299-7</a>
            <br>
            <br>
            <b>Effective Delay 50 in discounting:</b>
            <br>
            <br>
            Yoon, J. H., & Higgins, S. T. (2008). Turning k on its head:
            Comments on use of an ED50 in delay discounting research. <em>Drug
            and Alcohol Dependence, 95</em>(1), 169-172.
            <a href='https://doi.org/10.1016/j.drugalcdep.2007.12.011'>
            https://doi.org/10.1016/j.drugalcdep.2007.12.011</a>
            <br>
            <br>
            <b>Statistics in behavior analysis:</b>
            <br>
            <br>
            Young, M. E. (2018). A place for statistics in behavior analysis.
            <em>Behavior Analysis: Research and Practice, 18</em>(2),
            193-202.
            <a href='https://doi.org/10.1037/bar0000099'>
            https://doi.org/10.1037/bar0000099</a>
            <br>
            <br>
            "
          )
        )
      )
    )
  )
}


#' @export
body_server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$download_demand_wide <- shiny$downloadHandler(
      filename <- function() {
        paste("Shinybeez-template-demand-wide.csv")
      },
      content <- function(file) {
        file.copy("app/static/data/templates/template_demand_wide.csv", file)
      }
    )

    output$download_demand_long <- shiny$downloadHandler(
      filename <- function() {
        paste("Shinybeez-template-demand-long.csv")
      },
      content <- function(file) {
        file.copy("app/static/data/templates/template_demand_long.csv", file)
      }
    )

    output$download_demand_wide_group <- shiny$downloadHandler(
      filename <- function() {
        paste("Shinybeez-template-demand-wide-grouping.csv")
      },
      content <- function(file) {
        file.copy("app/static/data/templates/template_demand_wide_onegroup.csv", file)
      }
    )

    output$download_demand_long_group <- shiny$downloadHandler(
      filename <- function() {
        paste("Shinybeez-template-demand-long-grouping.csv")
      },
      content <- function(file) {
        file.copy("app/static/data/templates/template_demand_long_onegroup.csv", file)
      }
    )

    output$download_27mcq_wide <- shiny$downloadHandler(
      filename <- function() {
        paste("Shinybeez-template-27item-mcq-wide.csv")
      },
      content <- function(file) {
        file.copy("app/static/data/templates/template_mcq27_wide.csv", file)
      }
    )

    output$download_27mcq_long <- shiny$downloadHandler(
      filename <- function() {
        paste("Shinybeez-template-27item-mcq-long.csv")
      },
      content <- function(file) {
        file.copy("app/static/data/templates/template_mcq27_long.csv", file)
      }
    )
  })
}
