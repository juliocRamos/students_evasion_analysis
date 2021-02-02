# -*- coding: utf-8 -*-
"""
Created on Fri Jan 29 21:10:08 2021

@authors: julio.r and diego.p
"""

from datetime import datetime
from time import sleep
from sklearn import tree
import pandas as pd
import graphviz
import os

# generate a standard output for model prediction results
def generate_output(all_predictions):
    for i in range(len(all_predictions)):
        try:
            logdir = "predictions_" + datetime.now().strftime("%Y%m%d-%H%M%S")
            os.makedirs(logdir)

            if (os.path.exists(logdir)):
                predictions_path = ""+logdir+"/prediction_summary.txt"
                file = open(predictions_path, 'w')

                if len(all_predictions) == 0:
                    print("Nothing to write in file!!")

                # TODO: add auc and precision_score, K value for epoch
                # TODO: add csv, if possible
                file.write("=================RESULTS EPOCH "+str(i)+"==========================\n")
                file.write(str(all_predictions[i]["confusion_matrix"])+"\n\n")
                file.write(str(all_predictions[i]["classification_report"])+"\n\n")
                file.write("Accuracy: " + str(all_predictions[i]["model_accuracy"])+"\n\n")
                file.write("Epoch params: " + str(all_predictions[i]["epoch_params"])+"\n\n")
                file.write("====================END OF EPOCH==========================\n\n")
                sleep(1)
        except Exception as ex:
            raise(ex)
        finally:
            file.close()

def generate_output_csv(n_epoch, all_predictions):
    try:
        ## Create file name
        logfile = "predictions_results.csv"

        ## Verify if all_predictions variable has content
        if len(all_predictions) == 0:
            print("Nothing to write in file!!")
        
        if (os.path.exists(logfile)):
            dt = pd.read_csv(logfile)
        else:
            dt = pd.DataFrame(columns = ["epoch","model_accuracy",
                                         "f1_score","precision_score",
                                         "roc_score","epoch_best_params"])
        ## create Dict to output data
        for i in range(len(all_predictions)):
            dt2 = {
                "epoch": n_epoch,
                "model_accuracy": all_predictions[i]["model_accuracy"],
                "f1_score": all_predictions[i]["f1_score"],
                "precision_score": all_predictions[i]["precision_score"],
                "roc_score": all_predictions[i]["roc_score"],
                "epoch_best_params": all_predictions[i]["epoch_params"]
            }
        dt = dt.append(dt2, ignore_index=True)
        
        ## Write CSV File
        dt.to_csv(logfile, index = False)
        
    except Exception as ex:
        raise(ex)        
            
# Get best K (TEST)
def find_best_k(n_tests, X_train, X_test, y_train, y_test):
    errors = []
    all_key_errors = []
    for i in range(0, n_tests):
        clf = KNeighborsClassifier(weights="distance",
                                   n_neighbors=1+i, p = 1,
                                   n_jobs = 10)
        clf.fit(X_train, y_train)
        predictions = clf.predict(X_test)
        error = np.mean(predictions != y_test)
        
        if i > 0:
            all_key_errors.append([i, error])
        
        errors.append(error)
        
    plt.figure(figsize=(12, 6))
    plt.plot(range(0, 40), errors, color='red', linestyle='dashed', marker='o',
             markerfacecolor='blue', markersize=10)
    plt.title('Error Rate K Value')
    plt.xlabel('K Value')
    plt.ylabel('Mean Error')
    return(min(all_key_errors, key=lambda x: x[1])[0])
    
def plot_tree(clf, df):
    feature_names = df.drop("EVADIDO", axis = 1)
    feature_names = feature_names.columns
    class_names = ["EVADIDO","NÃO EVADIDO"]

    dot_data = tree.export_graphviz(clf, out_file = None,
                    feature_names=feature_names,
                    class_names=class_names,
                    filled=True, 
                    rounded=True,
                    special_characters=True,
                    precision=3)

    graph = graphviz.Source(dot_data)
    graph.format = "png"
    graph.render('dtree_render',view=True)
