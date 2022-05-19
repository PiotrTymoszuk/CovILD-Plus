# this script renders a word file with the figures and supplementary material

  insert_head()
  
# Exporting the figure chunks ------
  
  insert_msg('Figure chunks')

  insert_figure(paper_figures$symptoms, 
                paper_figures$lft, 
                paper_figures$ct, 
                paper_figures$cardio, 
                paper_figures$correlations, 
                paper_figures$clusters, 
                paper_figures$clust_psych, 
                file = './paper/markdown/figure_chunks.Rmd', 
                ref_names = stri_replace_all(names(paper_figures), fixed = '_', replacement = '-'), 
                captions = c('COVID-19 symptom recovery.', 
                             'Functional lung recovery.', 
                             'Radiological lung recovery.', 
                             'Cardiological recovery.', 
                             'Persistent symptoms and cardiopulmonary abnormalities and mobility, health self-perception, fatigue and stress scoring.', 
                             'Clusters of clinical and psychosocial COVID-19 recovery.', 
                             'Quality of life, fatigue and mental health rating in the COVID-19 recovery clusters.'), 
                add_extern_legend = TRUE, 
                append = FALSE)
  
# Exporting the supplementary figure chunks ------
  
  insert_msg('Supplementary Figure chunks')
  
  insert_figure(suppl_figures$symptoms1, 
                suppl_figures$symptoms2, 
                suppl_figures$lft, 
                suppl_figures$correl, 
                suppl_figures$clust_qc, 
                suppl_figures$clust_extra, 
                suppl_figures$sympt_risk, 
                suppl_figures$lft_risk, 
                suppl_figures$ct_risk, 
                suppl_figures$dysf_risk, 
                file = './paper/markdown/supplement_chunks.Rmd', 
                ref_names = stri_replace_all(names(suppl_figures), fixed = '_', replacement = '-'), 
                captions = c('Recovery of fatigue, sleep problems and dyspnea.', 
                             'Recovery from night sweating, cough and smell disorders.', 
                             'Changes in FEV1, FVC and DLCO during COVID-19 convalescence.', 
                             'Correlation of symptom number and dyspnea rating with the scoring of stress, fatigue and daily functioning.', 
                             'Development of COVID-19 recovery clusters.', 
                             'COVID-19 severity, demographic features, physical performance and mobility in the COVID-19 recovery clusters.', 
                             'Modeling of the persistent symptom risk at the 1-year post-COVID-19 follow-up.',
                             'Modeling of the persistent functional lung abnormality at the 1-year post-COVID-19 follow-up.', 
                             'Modeling of the persistent radiological lung abnormality at the 1-year post-COVID-19 follow-up.', 
                             'Modeling of the persistent diastolic dysfunction at the 1-year post-COVID-19 follow-up.'), 
                add_extern_legend = TRUE, 
                append = FALSE)

# rendering the figures and tables ------

  #insert_msg('Rendering the figures and tables')
  
 render('./paper/markdown/figures_and_tables.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
         output_dir = './paper') 

# rendering the supplementary material -----
  
  insert_msg('Rendering the supplementary material')
  
  render('./paper/markdown/supplementary_material.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
         output_dir = './paper') 
  
# END ------
  
  insert_tail()