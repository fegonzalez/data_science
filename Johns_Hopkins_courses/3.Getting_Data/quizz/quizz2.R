
## --------------------------------------------------------------------------

download_q2 <- function(filename = "getdata_data_ss06pid.csv")
{
    urlval<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
    download.file(url=urlval, destfile=filename, method="wget")
    return (filename);
}

## --------------------------------------------------------------------------

load_q2 <- function()
{
    dest_file_name <- "getdata_data_ss06pid.csv";

    if( ! is.element(dest_file_name, list.files()))
    {
        download_q2(dest_file_name);
    }
    library(data.table);
    acs <- fread(dest_file_name);
    return(acs);
}

## --------------------------------------------------------------------------

## Q2) Which of the following commands will select only the data for the
## probability weights pwgtp1 with ages less than 50?
##
## Q3) Using the same data frame you created in the previous problem, what is
## the equivalent function to unique(acs$AGEP)
q2q3<- function()
{
    acs <- load_q2();
    str(acs);

    ## install.packages("sqldf");
    library(sqldf);

    ## q2 Solution =
    q2_sol <- sqldf("select pwgtp1 from acs where AGEP < 50");

    ## q3 Solution =
    sq3_sol <- sqldf("select distinct AGEP from acs");

    return(acs);
}

## --------------------------------------------------------------------------

## Q4) How many characters are in the 10th, 20th, 30th and 100th lines of HTML
## from this page:
##
##  "http://biostat.jhsph.edu/~jleek/contact.html "
q4 <- function()
{
    url_name <- "http://biostat.jhsph.edu/~jleek/contact.html";
    stopifnot(nchar(url_name)==44); #testing: 44 characters

    conn <- url(url_name);
    lines <- readLines(conn, n = 100);
    length(lines)
    close(conn)
    retval <- c(nchar(lines[10]), nchar(lines[20]),
                nchar(lines[30]), nchar(lines[100]));
    print(retval);
    return(retval);

    ## q4 Solution =  [1] 45 31  7 25
}

## --------------------------------------------------------------------------

## Read this data set into R and report the sum of the numbers in the fourth of
## the nine columns.
##
## https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for
##
## Original source of the data:
## http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for
##
## (Hint this is a fixed width file format)
q5 <- function()
{
    dest_file_name <- "q5file.raw";

    if( ! is.element(dest_file_name, list.files()))
    {
        download_q5(dest_file_name);
    }

    ## row classigication
    col0sep <- nchar(" ");
    col1size <- nchar("03JAN1990");
    col1sep <- nchar("     ");
    col2size <- nchar("23.4");
    col2sep <- nchar("-");
    col3size <- nchar("0.4");
    col3sep <- nchar("     ");
    col4size <- nchar("25.1");
    col4sep <- nchar("-");
    col5size <- nchar("0.3");
    col5sep <- nchar("     ");
    col6size <- nchar("26.6");
    col6sep <- nchar(" ");
    col7size <- nchar("0.0");
    col7sep <- nchar("     ");
    col8size <- nchar("28.6");
    col8sep <- nchar(" ");
    col9size <- nchar("0.3");

    ## do check correctness
    total_line1 <-
        " 28MAR1990     25.7-0.4     27.5 0.2     27.8 0.3     28.8 0.5";
    partbypart_line1 <-  col0sep +
        col1size + col1sep +
            col2size + col2sep +
                col3size + col3sep +
                    col4size + col4sep +
                        col5size + col5sep +
                            col6size + col6sep +
                                col7size + col7sep +
                                    col8size + col8sep +
                                        col9size;
    stopifnot(nchar(total_line1)==partbypart_line1); #62
    ## check correctness done

    ## read.fwf params
    my_nrows <- -1; # -1 = all
    my_nskip <- 4;
    my_widths <- c(-col0sep,
        col1size,  -col1sep,
            col2size,  -col2sep,
                col3size,  -col3sep,
                    col4size,  -col4sep,
                        col5size,  -col5sep,
                            col6size,  -col6sep,
                                col7size,  -col7sep,
                                    col8size,  -col8sep,
                                        col9size);
    datatable <- read.fwf(dest_file_name,
                          skip=my_nskip,
                          n=my_nrows,
                          widths = my_widths);
    ## str(datatable);

    ## report the sum of the numbers in the fourth colum
    return(sum(datatable[4]));

    ## q5 Solution =  [1] 32426.7
}

## --------------------------------------------------------------------------

download_q5 <- function(filename = "q5file.raw")
{
    urlval<-"https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for";
    download.file(url=urlval, destfile=filename, method="wget")
    return (filename);
}

## --------------------------------------------------------------------------



## --------------------------------------------------------------------------

## Q1)
##
## STEP 1) Register an application in github:
##     "https://github.com/settings/applications/new"
##
## Application name:           curso_quizz2_q1
## Homepage URL:               http://github.com
## Authorization callback URL: http://localhost:1410 #CRITICAL
##
## Obtained:
##  Client ID       "91094c6db46bc6f30976"
##  Client Secret   "66b3945a4435cda493a867d820e5f1ff0738a5d0"
##
##
## STEP 2) Make the R script to get remote access to the application.
##
q1 <- function()
{
    # https://class.coursera.org/getdata-011/forum/thread?thread_id=16

    ## Using oauth_app & oauth2.0_token
    q1_regular();

    ## Using 'authenticate' over a PERSONAL_GITHUB_TOKEN
    q1_alt();
}


## Using 'authenticate' over a PERSONAL_GITHUB_TOKEN
q1_alt <- function()
{
    require(httr);
    require(httpuv);
    require(jsonlite);

    GET_URL_VALUE<- "https://api.github.com/users/jtleek/repos";
    the_appname <- "github";
    THE_PERSONAL_GITHUB_TOKEN <- "b36aaea3265ff56fca64a2708b4b3f2244b53d80";
    the_key <- THE_PERSONAL_GITHUB_TOKEN;
    the_pass <- "";

    ## 3.- Generate an oauth2.0 token.
    gtoken <- authenticate(the_key, 'x-oauth-basic');
    gtoken;
    ## the_user_url <- "https://github.com/fegonzalez";
    ## Just to test, use the correct URL for your code
    ## GET('https://api.github.com/fegonzalez">https://api.github.com/fegonzalez">https://api.github.com/fegonzalez', gtoken)


    ## 4. Use API
    req <- GET(GET_URL_VALUE, gtoken);
    stop_for_status(req);
    json1 = content(req)
    retval = jsonlite::fromJSON(toJSON(json1))


    ## head(colnames(retval))
    ## [1] "id"  "name"      "full_name" "owner"     "private"   "html_url"

    ## head(retval["name"])
    ##                      name
    ## 1                ballgown
    ## 2 capitalIn21stCenturyinR
    ## 3            dataanalysis
    ## 4           datascientist
    ## 5             datasharing    #  HERE!
    ## 6      datawomenontwitter
    retval$name[5]

    ## head(retval["created_at"])
    ##             created_at
    ## 1 2013-08-28T18:18:50Z
    ## 2 2014-05-27T20:38:31Z
    ## 3 2013-01-22T12:16:41Z
    ## 4 2012-06-24T14:36:20Z
    ## 5 2013-11-07T13:25:07Z
    ## 6 2014-09-09T15:49:57Z
    return(retval$created_at[5]);
}


## Using oauth_app & oauth2.0_token
q1_regular <- function()
{
    require(httr);
    require(httpuv);
    require(jsonlite);

    GET_URL_VALUE<- "https://api.github.com/users/jtleek/repos";
    the_appname <- "github";
    the_key <- "91094c6db46bc6f30976";
    the_pass <- "66b3945a4435cda493a867d820e5f1ff0738a5d0";

    ## Via OAuth Tokens
    ##     Alternatively, you can authenticate using personal access tokens or
    ##     OAuth tokens. To do so, provide the token as the username and
    ##     provide a blank password or a password of x-oauth-basic. If you’re
    ##     accessing the API via cURL, replace <token> with your OAuth token in
    ##     the following command:

    ## 1.- Find OAuth settings for github: http://developer.github.com/v3/oauth/
    ## Access Token
    ## Access tokens are credentials used to access protected resources.  An
    ## access token is a string representing an authorization issued to the
    ## client.  The string is usually opaque to the client.  Tokens
    ## represent specific scopes and durations of access, granted by the
    ## resource owner, and enforced by the resource server and authorization
    ## server.

    ## Protocol Endpoints [REF 13.2]
    oauth_endpoints(the_appname);

    ## 2.- Insert your client ID and secret below - if secret is omitted, it
    ## will look it up in the GITHUB_CONSUMER_SECRET environmental variable.
    #myapp <- oauth_app(appname=my_appname, key=ClientID);
    ## Couldn't find secret in environment variable GITHUB_CONSUMER_SECRET
    myapp <- oauth_app(appname=the_appname,
                       key=the_key,
                       secret=the_pass);

    ## 3.- Generate an oauth2.0 token.
    ##
    ## This is the final object in the OAuth dance - it encapsulates the app,
    ## the endpoint, other parameters and the received credentials.  It is a
    ## reference class so that it can be seamlessly updated (e.g.  using
    ## ‘$refresh()’) when access expires.

    ## the success/fail authentication and authorization result is communicated
    ## over the 'localhost' network on your host to the R runtimeYou can verify
    ## this using the 'netstat' command (available on MacOS terminal, Windows
    ## cmd.exe, and Linux command lines);
    ##
    ## doing a "netstat -a | grep LISTEN"
    ##
    ## while the R command window is displaying
    ##
    ## Waiting for authentication in browser...
    ##
    ## You should see an entry that looks similar to
    ##
    ## TCP 127.0.0.1:1410 nate-pc:0 LISTENING
    ##
    ## This port will be closed when the oauth2.0_token() returns with a
    ## success/fail message.

    github_token <- oauth2.0_token(oauth_endpoints(the_appname), myapp,
                       cache=FALSE); # CRITICAL param., auth failure otherwise
    github_token;

    ## 4. Use API
    gtoken <- config(token = github_token)
    gtoken;
    req <- GET(GET_URL_VALUE, gtoken);
    stop_for_status(req)
    ## content(req)
    json1 = content(req)
    retval = jsonlite::fromJSON(toJSON(json1))
    retval$name[5]
    return(retval$created_at[5]);
}

## q1 Solution = "2013-11-07T13:25:07Z"

## q2 Solution = q2_sol <- sqldf("select pwgtp1 from acs where AGEP < 50");

## q3 Solution = sq3_sol <- sqldf("select distinct AGEP from acs");

## q4 Solution =  [1] 45 31  7 25

## q5 Solution = [1] 32426.7
