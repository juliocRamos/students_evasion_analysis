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
def generate_output(all_predictions, logdir = "", model = "", run = 0):
    for pred in all_predictions:
        try:
            if (os.path.exists(logdir)):
                predictions_dirs = "prediction_detailed"
                full_path = os.path.join(logdir, model, predictions_dirs)
                if os.path.exists(full_path) != True:
                    os.makedirs(full_path)

                prediciton_file = full_path + "/prediction_summary_" \
                + "run_" + str(run) + "_" \
                + "splt_" + str(pred["n_splits"]) +".txt"
                file = open(prediciton_file, 'w')

                if len(pred) == 0:
                    print("Nothing to write in file!!")

                file.write("=================RESULTS EPOCH ==========================\n")
                file.write(str(pred["confusion_matrix"])+"\n\n")
                file.write(str(pred["classification_report"])+"\n\n")
                file.write("Splits: " + str(pred["n_splits"])+"\n\n")
                file.write("Accuracy: " + str(pred["model_accuracy"])+"\n\n")
                file.write("F1 score: " + str(pred["f1_score"])+"\n\n")
                file.write("Precision: " + str(pred["precision_score"])+"\n\n")
                file.write("ROC: " + str(pred["roc_score"])+"\n\n")
                file.write("Max error: " + str(pred["max_error"])+"\n\n")
                file.write("Mean squared err.: " + str(pred["mean_squared_error"])+"\n\n")
                file.write("Mean abs. err.: " + str(pred["mean_absolute_error"])+"\n\n")
                file.write("Epoch params: " + str(pred["epoch_params"])+"\n\n")
                file.write("====================END OF EPOCH==========================\n\n")
                sleep(1)
        except Exception as ex:
            raise(ex)
        finally:
            file.close()

def generate_output_csv(all_predictions, logdir = "", model = ""):
    try:
        ## Create file name
        file_name = "predictions_results.csv"
        results_path = os.path.join(logdir, model, file_name)

        ## Verify if all_predictions variable has content
        if len(all_predictions) == 0:
            print("Nothing to write in file!!")

        if (os.path.exists(results_path)):
            dt = pd.read_csv(results_path)
        else:
            dt = pd.DataFrame(columns = ["n_splits", "model_accuracy", "f1_score",
                                         "precision_score", "roc_score",
                                         "recall_score", "max_error",
                                         "mean_squared_error", "mean_absolute_error",
                                         "epoch_best_params"])
        
        
        ## insert in Dict the output data
        for pred in all_predictions:
            ## Create Vector Dict
            dt2 = {
                "n_splits": pred["n_splits"],
                "model_accuracy": pred["model_accuracy"],
                "f1_score": pred["f1_score"],
                "precision_score": pred["precision_score"],
                "roc_score": pred["roc_score"],
                "recall_score": pred["recall_score"],
                "max_error": pred["max_error"],
                "mean_squared_error": pred["mean_squared_error"],
                "mean_absolute_error": pred["mean_absolute_error"],
                "epoch_best_params": pred["epoch_params"]
            }
            ## Append dict in the dataframe
            dt = dt.append(dt2, ignore_index=True)
        
        ## Write CSV File
        dt.to_csv(results_path, index = False)
        
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
