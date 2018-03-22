#!groovy

pipeline {

	// Run in docker container
	agent {
        docker {
            image   'friesischscott/gitlab-ci-matlab'
            args    '-v /opt/MATLAB/R2017b/:/usr/local/MATLAB/from-host -v /home/jenkins/.matlab/R2017b:/.matlab/R2017b --mac-address=2c:60:0c:e3:7e:8c'
		}
	}
	
	stages {
		// Run all tests specified in run_tests.m
		stage ('Start') {
            steps {
                // send build started notifications
                slackSend (color: '#D4DADF', message: "STARTED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]' (${env.BUILD_URL})")
            }
		}
		stage("Test") {
		    parallel {		        
		        stage("Integration") {
		            steps {
		                sh 'matlab -r run_integration_tests'
		            }
		            post {
		                success {
		                    // archive and track test results
                            archive "integrationResults.xml"
                            junit "integrationResults.xml"
		                }
		            }
		        }
		    }
		}
	}

	post {
        success {
            // scan for open tasks 
	        step([$class: 'TasksPublisher', high: '', ignoreCase: true, low: '', normal: 'TODO, TO DO', pattern: '**/*.m'])
	        // notify slack
            slackSend (color: '#BDFFC3', message: "SUCCESSFUL: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]' (${env.BUILD_URL})")
        }
        
        unstable {
            // notify slack
            slackSend (color: '#FFFE89', message: "UNSTABLE: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]' (${env.BUILD_URL})")
        }

		failure {
		    // notify slack
		    slackSend (color: '#FF9FA1', message: "FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]' (${env.BUILD_URL})")
		    // send out emails
			emailext(
		    body: '${TEMPLATE, file="openCossan.template"}',
		    mimeType: 'text/html',
            subject: '$PROJECT_NAME - Build # $BUILD_NUMBER - $BUILD_STATUS!',
            to: emailextrecipients([[$class: 'CulpritsRecipientProvider']]))
		}
	}
}