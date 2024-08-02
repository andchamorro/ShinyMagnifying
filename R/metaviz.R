#################################################
######## meta-visualization function
##### Ma, R., Sun, E. D., & Zou, J. (2023). 
##### A spectral method for assessing and combining multiple data visualizations.
##### Nature communications, 14(1), 780. https://doi.org/10.1038/s41467-023-36492-2
#################################################

# ensemble.viz: using spectral method for quantifying visualization quality and generating meta-visualization.
# data.list: a list of 2-dimensionoal embeddings, which is created by candidate.visual.
# name.method: the names of the candidate visualizations.
# original.data: an option to use the original data for the quality assessment, instead of using eigenscores.
# Output: a list containing (1) a meta-distance for meta-visualization, (2) eigenscores for candidate visualizations,
#         (3) names of the method ordered by averaged eigenscores.



#this was used for running time evaluation for n>10000 data
ensemble_viz <- function(data.list, name.method=NA, original.data=NA){
  
  
  n=dim(data.list[[1]])[1]
  K=length(data.list)
  
  if(is.na(original.data)){
    ########## obtain weights
    
    ensemble.mat = matrix(ncol=n,nrow=n)
    weight = matrix(ncol=K,nrow=n)
    for(j in 1:n){
      
      local.dist=matrix(ncol=n,nrow=K)
      for(i in 1:K){
        local.dist[i,]= sqrt(rowSums(t(data.list[[i]][j,]-t(data.list[[i]]))^2))
      }
      
      comp.mat = matrix(ncol=K, nrow=K)
      embed.mat.norm = list()
      for(i in 1:K){
        for(k in 1:K){
          comp.mat[i,k] = sum(local.dist[i,]*local.dist[k,])/sqrt(sum(local.dist[k,]^2))/sqrt(sum(local.dist[i,]^2))
        }
        embed.mat.norm[[i]] = local.dist[i,]/sqrt(sum(local.dist[i,]^2))
      }
      weight[j,] = abs(eigen(comp.mat)$vectors[,1])
      
      
      ensemble.mat[,j] = apply(do.call(cbind, embed.mat.norm), 1, weighted.mean, w = weight[j,])*sum(weight[j,])
      if(j/1000==floor(j/1000)){
        print(paste0(j," samples done!"))
      }
    }
    
    
    return(list(ensemble.dist.mat=(ensemble.mat+t(ensemble.mat))/2, eigenscore=weight, 
                name.method=name.method[order(colMeans(weight), decreasing = T)]))
    
  }else{
    
    ensemble.mat = matrix(ncol=n,nrow=n)
    weight = matrix(ncol=K,nrow=n)
    data.mat= as.matrix(dist(original.data))
    for(j in 1:n){
      
      local.dist=matrix(ncol=n,nrow=K)
      for(i in 1:K){
        local.dist[i,]= sqrt(rowSums(t(data.list[[i]][j,]-t(data.list[[i]]))^2))
      }
      
      eigen.score = c()
      embed.mat.norm = list()
      for(i in 1:K){
        eigen.score[i]=sum(data.mat[,j]*local.dist[i,])/sqrt(sum(data.mat[,j]^2))/sqrt(sum(local.dist[i,]^2))
        embed.mat.norm[[i]] = local.dist[i,]/sqrt(sum(local.dist[i,]^2)) 
      }
      
      eigen.score[which(eigen.score<0)]=0
      weight[j,] = eigen.score^2/sum(eigen.score^2)
      
      ensemble.mat[,j] = apply(do.call(cbind, embed.mat.norm), 1, weighted.mean, w = weight[j,])*sum(weight[j,])
      
    }
    
    return(list(ensemble.dist.mat=(ensemble.mat+t(ensemble.mat))/2, eigenscore=weight, 
                name.method=name.method[order(colMeans(weight), decreasing = T)]))
    
  }  
}

emsembleViz <- function(seurat_obj, name_method = NULL, original_data = NULL){
  # Perform dimensionality reduction using Seurat methods:
  # Assuming your Seurat object is named 'seurat_obj'
  
  # PCA
  if (!("pca" %in% Reductions(seurat_obj))) {
    seurat_obj <- RunPCA(seurat_obj, features = VariableFeatures(object = seurat_obj))
  } 
  pca_embeddings <- Embeddings(seurat_obj, "pca")[, 1:2]
  
  # ICA
  if (!("ica" %in% Reductions(seurat_obj))) {
    seurat_obj <- RunICA(seurat_obj, features = VariableFeatures(object = seurat_obj))
  } 
  ica_embeddings <- Embeddings(seurat_obj, "ica")[, 1:2]
  
  # UMAP
  if (!("umap" %in% Reductions(seurat_obj))) {
    seurat_obj <- RunUMAP(seurat_obj, dims = 1:10)
  }
  umap_embeddings <- Embeddings(seurat_obj, "umap")
  
  # Check for t-SNE
  if (!("tsne" %in% Reductions(seurat_obj))) {
    seurat_obj <- RunTSNE(seurat_obj, dims = 1:10)
  }
  tsne_embeddings <- Embeddings(seurat_obj, "tsne")
  
  # Combine the embeddings into a list:
    data_list <- list(
      PCA = pca_embeddings,
      ICA = ica_embeddings,
      UMAP = umap_embeddings,
      tSNE = tsne_embeddings
    )
  # Apply the ensemble.viz function:
    ensemble_result <- ensemble.viz(data_list, names(data_list))
    ensemble_obj <- CreateDimReducObject(
      embeddings = ensemble_result$ensemble.dist.mat, 
      key = "metaviz",
      assay = DefaultAssay(seurat_obj),
      misc = list(eigenscore = ensemble_result$eigenscore, 
                  name.method = ensemble_result$name.method))
  # Add the new reduction to the Seurat object
    seurat_obj[["METAVIZ"]] <- ensemble_obj
}