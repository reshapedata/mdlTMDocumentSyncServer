#' 采购退料同步至数据中台
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param wms_token
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_sync_dmsServer()
purchaseReturn_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_purchaseReturn_fyear_wms = tsui::var_text('txt_purchaseReturn_fyear_wms')

  txt_purchaseReturn_fmonth_wms = tsui::var_text('txt_purchaseReturn_fmonth_wms')
  shiny::observeEvent(input$btn_purchaseReturn_sync_dms,{
    fyear =txt_purchaseReturn_fyear_wms()
    fmonth=txt_purchaseReturn_fmonth_wms()


    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_purchaseReturn_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)
      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_purchaseReturn_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_pur_mrb',r_object = data,append = TRUE)
      tsui::pop_notice("上传完成")

    }





  })


}


#' 采购退料同步至ERP
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_sync_erpServer()
purchaseReturn_sync_erpServer <- function(input,output,session,dms_token) {

  txt_purchaseReturn_fbillno_sync_erp = tsui::var_text('txt_purchaseReturn_fbillno_sync_erp')
  shiny::observeEvent(input$btn_purchaseReturn_sync_erp,{
    tsui::pop_notice(txt_purchaseReturn_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS采购退料按单据编号查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_fbillno_view_dmsServer()
purchaseReturn_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  txt_purchaseReturn_fbillno_view = tsui::var_text('txt_purchaseReturn_fbillno_view')
  shiny::observeEvent(input$btn_purchaseReturn_fbillno_view_dms,{

    fbillno=txt_purchaseReturn_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_purchaseReturn_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_purchaseReturn',data = data)
    tsui::run_download_xlsx(id = 'dl_purchaseReturn_fbillno_dms',data = data,filename = "采购退料按单查询数据.xlsx")




  })


}

#' 采购退料按日期查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_date_view_dmsServer()
purchaseReturn_date_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_purchaseReturn= tsui::var_date('date_tm_purchaseReturn')

  shiny::observeEvent(input$btn_purchaseReturn_date_view_dms,{
    fdate=date_tm_purchaseReturn()
    data=mdlTMDocumentSyncPkg::Tmdms_purchaseReturn_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_purchaseReturn',data = data)

  })


}

#' 采购退料按日期范围查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_dateRange_view_dmsServer()
purchaseReturn_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_purchaseReturn_FStartDate= tsui::var_date('date_tm_purchaseReturn_FStartDate')

  date_tm_purchaseReturn_FEndDate= tsui::var_date('date_tm_purchaseReturn_FEndDate')

  shiny::observeEvent(input$btn_purchaseReturn_dateRange_view_dms,{
    FStartDate=date_tm_purchaseReturn_FStartDate()

    FEndDate=date_tm_purchaseReturn_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_purchaseReturn_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_purchaseReturn',data = data)

    tsui::run_download_xlsx(id = 'dl_purchaseReturn_dateRange_dms',data = data,filename = "采购退料单数据.xlsx")





  })


}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param wms_token
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturnServer()
purchaseReturnServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::purchaseReturn_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::purchaseReturn_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::purchaseReturn_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseReturn_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseReturn_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
