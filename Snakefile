## Contributors: Chiara Aina
## (GitHub: @chiaraaina, Email: chiara.aina@uzh.ch

##############################################
## Misc
##############################################

configfile: 'config.yaml'

# Run R with the following options
# do not save workspace after session
# do not restore previously saved objects
# print all information while running the process

run_R = "Rscript --no-save --no-restore --verbose"

##############################################
## Get all input filenames
##############################################

GRAPHS_MEAN = ['accept_mean', 'f_accept_mean', 'm_accept_mean', 'offer_mean', 'f_offer_mean', 'm_offer_mean']
GRAPHS_TIME = ['frustration', 'expectations']

##############################################
## Build Rules
##############################################
rule allRules:
    input:
        config['in_latex']+'report.md',
        'Aina_AdditionalAnalysis.pdf',
        expand(config['out_graphs']+"{iGraph}.jpeg", iGraph = GRAPHS_TIME),
        expand(config['out_graphs']+"{iGraph}.jpeg", iGraph = GRAPHS_MEAN),
        config['out_data']+'cleaned_data_all.csv',
        config['out_data']+'cleaned_data.csv'

## makePDF: create a report of the project
rule makePDF:
    input:
        Rmd = config['in_latex'] + 'report.Rmd',
        script = config['in_code'] + 'knit.R',
        graphs = expand(config['out_graphs']+'{iGraph}.jpeg', iGraph = GRAPHS_MEAN + GRAPHS_TIME)
    output:
        md = config['in_latex']+'report.md',
        pdf = 'Aina_AdditionalAnalysis.pdf'
    run:
        shell("{run_R} {input.script}")
        shell("pandoc {output.md} -o {output.pdf}")

## responseTime: create graphs for response time of B-subjects
rule responseTime:
    input:
        script = config['in_code']+"response_time.R",
        data = config['out_data']+ "cleaned_data.csv"
    output:
        expand(config['out_graphs']+"{iGraph}.jpeg", iGraph = GRAPHS_TIME)
    params:
        config['out_graphs']
    shell:
        "{run_R} {input.script} {input.data} {params}; rm Rplots.pdf"

## makeGraphs: create graphs with pvalue of difference in means
rule makeGraphs:
    input:
        script = config['in_code']+"graphs.R",
        data = config['out_data']+ "cleaned_data.csv"
    output:
        expand(config['out_graphs']+"{iGraph}.jpeg", iGraph = GRAPHS_MEAN)
    params:
        config['out_graphs']
    shell:
        "{run_R} {input.script} {input.data} {params}"

## cleanData: clean the data and create relevant variables
rule cleanData:
    input:
        script = config['in_code'] + 'clean_data.R',
        rawData = config['in_data'] + 'fake_data.csv'
    output:
        out1 = config['out_data']+'cleaned_data_all.csv',
        out2 = config['out_data']+'cleaned_data.csv'
    shell:
        "{run_R} {input.script} {input.rawData} {output.out1} {output.out2}"

## fakeData: from original dataset to a fake dataset
# rule fakeData:
#     input:
#         script = config['in_code'] + 'fake_data.R',
#         rawData = config['in_data'] + 'final_psycho.dta'
#     output:
#         config['in_data']+'fake_data.csv',
#     shell:
#         "{run_R} {input.script} {input.rawData} {output}"


##############################################
## Help Rule
##############################################`
rule help:
    input:
        'Snakefile'
    shell:
        'sed -n "s/^##//p" {input}'

##############################################
## Cleaning Rules
##############################################`

rule clean:
    shell:
        "find ./output -type f -delete; \
        rm -fv *.pdf"
