#'checkingIntegrationTable
#'
#' This function checks whether duplicates exist in the integrationTable and allows to highlight potential mistakes in the integrationTable
#' @param integrationTable a table that must contain several columns: "compo" for the composition, "name" for the lipid species; "mz" for the theoretical exact mass/z (warnings: this exact mass has to vary depending on the mode NEG/POS of the acquisition);
#' @param duplicatedName can be 'name' (default) or 'compo'. Detects if duplicated in this modality exists.
#' @examples
#' data(integrationTable)
#' checkingIntegrationTable(integrationTable)
#'@export
checkingIntegrationTable=function(integrationTable,duplicatedName="name",byClass=TRUE)
{
  integrationTablePos2=integrationTable
  print("Duplicated ---------")
  nom_mol=duplicatedName
  integrationTablePos3=integrationTablePos2[!is.na(integrationTablePos2[,nom_mol]),]
  dup=integrationTablePos3[duplicated(integrationTablePos3[,nom_mol]),]
  wrongIntegrationTable=FALSE
  if(dim(dup)[1]!=0)
  {
    for(i in 1:dim(dup)[1]) # pour toutes les lignes dupliquées
    {
      compo_i=dup[i,nom_mol]
      lignesDupliquees=integrationTablePos3[integrationTablePos3[,nom_mol]==compo_i,]
      if(byClass)
      {
        if(any(duplicated(lignesDupliquees[,"class"])))
        {
          print(lignesDupliquees)
          print("----------")
          wrongIntegrationTable=TRUE
        }
      }
      if(!byClass)
      {
        wrongIntegrationTable=TRUE
        print(lignesDupliquees)
        print("----------")
      }
    }
  }
  return(wrongIntegrationTable)
}
#integrationTable=data.frame(name=c("a","a","b","c","a"),class=c("r","r","r","s","s"),mz=1:5)
#integrationTable=data.frame(name=c("a","a","b","c","a"),class=c("r","t","r","s","s"),mz=1:5)
#checkingIntegrationTable(integrationTable)
