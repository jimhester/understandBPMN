

#' @title A function for creating internal documents
#' @description Is used for creating xml documents which nearly every function of this package needs as an input
#' @param bpmn_file file path of the BPMN file
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an object containing the xml document
#' @examples 
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' create_internal_doc(file_path)
#' @export
create_internal_doc <- function (bpmn_file, signavio = FALSE) {
  if (signavio) {
    doc <- xmlInternalTreeParse(bpmn_file)
  } else {
    internal_doc <- XML::xmlParse(file = bpmn_file)
    return (internal_doc)
  }
}

namespace <- function(xml_internal_doc) {
  ns <- getDefaultNamespace(xml_internal_doc)[[1]]$uri
  names(ns)[1] <- "xmlns"
}

# Calculate number of exclusive gateways
number_XOR_gateways <-
  function (xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      xor_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//bpmn:exclusiveGateway | //exclusiveGateway")
    } else {
      xor_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:exclusiveGateway",
                   namespace(xml_internal_doc))
    }
    return(length(xor_gateway_nodes))
  }

# Calculate number of parallel gateways
number_AND_gateways <-
  function (xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      and_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//bpmn:parallelGateway | //parallelGateway")
    } else {
      and_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:parallelGateway",
                   namespace(xml_internal_doc))
    }
    return(length(and_gateway_nodes))
  }

# Calculate number of inclusive gateways
number_OR_gateways <-
  function (xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      or_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//bpmn:inclusiveGateway | //inclusiveGateway")
    } else {
      or_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:inclusiveGateway",
                   namespace(xml_internal_doc))
    }
    return(length(or_gateway_nodes))
  }

# Calculate number of complex gateways
number_complex_gateways <-
  function(xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      complex_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//bpmn:complexGateway | //complexGateway")
    } else {
      complex_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:complexGateway",
                   namespace(xml_internal_doc))
    }
    return(length(complex_gateway_nodes))
  }

# Calculate number of event based gateways
number_event_based_gateways <-
  function(xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      event_based_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//bpmn:eventBasedGateway | //eventBasedGateway")
    } else {
      event_based_gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:eventBasedGateway",
                   namespace(xml_internal_doc))
    }
    return(length(event_based_gateway_nodes))
  }

# Return a table of all ids of a certain node type
node_ids <-
  function(xml_internal_doc,
           type,
           sequential = FALSE,
           signavio = FALSE) {
    if (!signavio) {
      nodes <- getNodeSet(xml_internal_doc, type)
    } else {
      nodes <-
        getNodeSet(xml_internal_doc, type, namespace(xml_internal_doc))
    }
    node_ids <- unlist(xmlApply(nodes, xmlGetAttr, name = "id"))
    if (sequential) {
      join_nodes <- join_gateways(xml_internal_doc, type, signavio)
      split_nodes <- split_gateways(xml_internal_doc, type, signavio)
      connector_nodes <- unique(c(join_nodes, split_nodes))
      if (length(connector_nodes) != 0)
        node_ids <- node_ids[!(node_ids %in% connector_nodes)]
    }
    return(node_ids)
  }

#' @title Task names
#' @description A function which returns the task names together with the task ids
#' @param xml_internal_doc document object created using the create_internal_document function
#' @param filter_non_connector_activities attribute indicating whether non connector activities should be filtered. The default value is FALSE.
#' @param signavio boolean which indicates whether the file stems from signavio
#' @return an object containing a table with the IDs and tasknames
#' @examples 
#' \dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
#' task_names(create_internal_doc(file_path))
#' @export
# Return a table with all ids and task names of all task nodes
task_names <-
  function(xml_internal_doc,
           filter_non_connector_activities = FALSE,
           signavio = FALSE) {
    if (!signavio) {
      task_nodes <-
        getNodeSet(
          xml_internal_doc,
          "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
          //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
          //bpmn:subProcess | //bpmn:callActivity | //task"
        )
    } else {
      task_nodes <-
        getNodeSet(
          xml_internal_doc,
          "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
          //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
          //xmlns:subProcess | //xmlns:callActivity",
          namespace(xml_internal_doc)
        )
    }
    if (filter_non_connector_activities) {
      if (!signavio) {
        join_activities <-
          join_gateways(
            xml_internal_doc,
            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task"
          )
        split_activities <-
          split_gateways(
            xml_internal_doc,
            "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
            //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
            //bpmn:subProcess | //bpmn:callActivity | //task"
          )
      } else {
        join_activities <-
          join_gateways(
            xml_internal_doc,
            "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity",
            TRUE
          )
        split_activities <-
          split_gateways(
            xml_internal_doc,
            "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
            //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
            //xmlns:subProcess | //xmlns:callActivity",
            TRUE
          )
      }
      connector_activities <-
        unique(c(join_activities, split_activities))
    }
    task_id <- unlist(xmlApply(task_nodes, xmlGetAttr, name = "id"))
    name_task_list <- xmlApply(task_nodes, xmlGetAttr, name = "name")
    name_task_list <- lapply(name_task_list, function(name) {
      name[is.null(name)] <- " "
      return (name)
    })
    task_names <- unlist(name_task_list)
    name_id <- as.data.frame(cbind(task_id, task_names))
    if (filter_non_connector_activities) {
      name_id <- name_id %>%
        filter(!(task_id %in% connector_activities))
    }
    return(name_id)
    }

# Calculate the number of tasks
number_tasks <- function (xml_internal_doc, signavio = FALSE) {
  if (!signavio)
    task_nodes <-
      getNodeSet(
        xml_internal_doc,
        "//bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
        //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
        //bpmn:subProcess | //bpmn:callActivity | //task"
      )
  else
    task_nodes <-
      getNodeSet(
        xml_internal_doc,
        "//xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
        //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
        //xmlns:subProcess | //xmlns:callActivity",
        namespace(xml_internal_doc)
      )
  total <- length(task_nodes)
  return(total)
}

# Calculate the number of events
number_events <- function (xml_internal_doc, signavio = FALSE) {
  if (!signavio) {
    event_nodes <-
      getNodeSet(
        xml_internal_doc,
        "//bpmn:startEvent | //bpmn:messageStartEvent | //bpmn:timerStartEvent |
        //bpmn:conditionalStartEvent | //bpmn:endEvent | //bpmn:messageEndEvent |
        //bpmn:terminateEndEvent | //bpmn:escalationEndEvent | //bpmn:errorEndEvent |
        //bpmn:compensationEndEvent | //bpmn:signalEndEvent | //bpmn:intermediateCatchEvent |
        //bpmn:intermediateThrowEvent | //bpmn:boundaryEvent | //startEvent | //endEvent | //intermediateEvent"
      )
  } else {
    event_nodes <-
      getNodeSet(
        xml_internal_doc,
        "//xmlns:startEvent | //xmlns:messageStartEvent | //xmlns:timerStartEvent |
        //xmlns:conditionalStartEvent | //xmlns:endEvent | //xmlns:messageEndEvent |
        //xmlns:terminateEndEvent | //xmlns:escalationEndEvent | //xmlns:errorEndEvent |
        //xmlns:compensationEndEvent | //xmlns:signalEndEvent | //xmlns:intermediateCatchEvent |
        //xmlns:intermediateThrowEvent | //xmlns:boundaryEvent",
        namespace(xml_internal_doc)
      )
  }
  total <- length(event_nodes)
  return(total)
}

# Calculate the number of sequence flows
number_sequence_flows <-
  function (xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      sequence_flow_nodes <-
        getNodeSet(xml_internal_doc, "//bpmn:sequenceFlow | //sequenceFlow")
    } else {
      sequence_flow_nodes <-
        getNodeSet(xml_internal_doc,
                   "//xmlns:sequenceFlow",
                   namespace(xml_internal_doc))
    }
    return(length(sequence_flow_nodes))
  }

# Calculate the the total number of incoming and outgoing sequence flows of all gateways
total_io_flows_gateways <-
  function (xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      io_flows <-
        number_io_flows_gateway(
          xml_internal_doc,
          gateway_type = "//bpmn:exclusiveGateway | //bpmn:parallelGateway |
          //bpmn:inclusiveGateway | //bpmn:eventBasedGateway | //bpmn:complexGateway |
          //exclusiveGateway | //parallelGateway |
          //inclusiveGateway | //eventBasedGateway | //complexGateway"
        )
    } else {
      io_flows <-
        number_io_flows_gateway(
          xml_internal_doc,
          gateway_type = "//xmlns:exclusiveGateway | //xmlns:parallelGateway |
          //xmlns:inclusiveGateway | //xmlns:eventBasedGateway | //xmlns:complexGateway",
          TRUE
        )
    }
    total_flows <- sum(io_flows)
    return(total_flows)
  }

# Returns the number of outgoing and incoming sequence flows of the gateway with the most incoming and outgoing flows
max_io_flows_gateways_activities <-
  function (xml_internal_doc, signavio = FALSE) {
    if (!signavio) {
      io_flows <-
        number_io_flows_gateway(
          xml_internal_doc,
          gateway_type = "//bpmn:exclusiveGateway | //bpmn:parallelGateway |
          //bpmn:inclusiveGateway | //bpmn:eventBasedGateway | //bpmn:complexGateway |
          //exclusiveGateway | //parallelGateway |
          //inclusiveGateway | //eventBasedGateway | //complexGateway | //bpmn:task | //bpmn:sendTask | //bpmn:receiveTask |
          //bpmn:manualTask | //bpmn:businessRuleTask | //bpmn:userTask | //bpmn:scriptTask |
          //bpmn:subProcess | //bpmn:callActivity | //task"
        )
    } else {
      io_flows <-
        number_io_flows_gateway(
          xml_internal_doc,
          gateway_type = "//xmlns:exclusiveGateway | //xmlns:parallelGateway |
          //xmlns:inclusiveGateway | //xmlns:eventBasedGateway | //xmlns:complexGateway | //xmlns:task | //xmlns:sendTask | //xmlns:receiveTask |
          //xmlns:manualTask | //xmlns:businessRuleTask | //xmlns:userTask | //xmlns:scriptTask |
          //xmlns:subProcess | //xmlns:callActivity",
          TRUE
        )
    }
    if (length(io_flows) == 0)
      return (0)
    else {
      if (max(io_flows) == 2)
        return (0)
      else
        return(max(io_flows))
    }
  }

# Returns the sum of the incoming and outgoing sequence flows of all gateways of a certain type
number_io_flows_gateway <-
  function (xml_internal_doc,
            gateway_type,
            signavio = FALSE) {
    if (!signavio) {
      incoming <- number_incoming_flows(xml_internal_doc, gateway_type)
      outgoing <- number_outgoing_flows(xml_internal_doc, gateway_type)
    } else {
      incoming <-
        number_incoming_flows(xml_internal_doc, gateway_type, signavio)
      outgoing <-
        number_outgoing_flows(xml_internal_doc, gateway_type, signavio = signavio)
    }
    
    io_flows <- incoming + outgoing
    
    return(io_flows)
  }

# Returns the sum of the incoming and outgoing sequence flows of all activities with a certain ids
number_io_flows_activities_with_id <-
  function(xml_internal_doc,
           activity_ids,
           signavio = FALSE) {
    xpath_expression_look_up_activities <-
      paste(paste("//*[@id='", activity_ids, "']", sep = ""), collapse = " | ")
    if (!signavio) {
      activities <-
        getNodeSet(xml_internal_doc, xpath_expression_look_up_activities)
    } else {
      activities <-
        getNodeSet(
          xml_internal_doc,
          xpath_expression_look_up_activities,
          namespace(xml_internal_doc)
        )
    }
    activities_incoming_sf <- xmlApply(activities,
                                       xmlElementsByTagName,
                                       name = "incoming",
                                       recursive = FALSE)
    activities_incoming_sf <-
      unlist(map(activities_incoming_sf, length))
    activities_outgoing_sf <- xmlApply(activities,
                                       xmlElementsByTagName,
                                       name = "outgoing",
                                       recursive = FALSE)
    activities_outgoing_sf <-
      unlist(map(activities_outgoing_sf, length))
    return(sum(activities_incoming_sf) + sum(activities_outgoing_sf))
    
  }

# Returns the sum of the incoming sequence flows of all gateways of a certain type
number_incoming_flows <-
  function (xml_internal_doc,
            gateway_type,
            signavio = FALSE) {
    if (!signavio) {
      gateway_nodes <- getNodeSet(xml_internal_doc, gateway_type)
    } else {
      gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   gateway_type,
                   namespace(xml_internal_doc))
    }
    
    #Check all children of the gateway node having the name incoming
    gateway_nodes_incoming <- xmlApply(gateway_nodes,
                                       xmlElementsByTagName,
                                       name = "incoming",
                                       recursive = FALSE)
    gateway_nodes_incoming <-
      unlist(map(gateway_nodes_incoming, length))
    return(gateway_nodes_incoming)
  }

# Returns the sum of the  outgoing sequence flows of all gateways of a certain type
number_outgoing_flows <-
  function (xml_internal_doc,
            gateway_type,
            filter_split = FALSE,
            signavio = FALSE) {
    if (!signavio) {
      gateway_nodes <- getNodeSet(xml_internal_doc, gateway_type)
    } else {
      gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   gateway_type,
                   namespace(xml_internal_doc))
    }
    
    if (filter_split) {
      split_gateways <-
        split_gateways(xml_internal_doc, gateway_type, signavio)
      id_gateway_nodes <-
        unlist(xmlApply(gateway_nodes, xmlGetAttr, name = "id"))
      gateway_node_indices <-
        which(id_gateway_nodes %in% split_gateways)
      gateway_nodes <- gateway_nodes[gateway_node_indices]
    }
    #Check all children of the gateway node having the name outgoing
    gateway_nodes_outgoing <- lapply(gateway_nodes,
                                     xmlElementsByTagName,
                                     name = "outgoing",
                                     recursive = FALSE)
    gateway_nodes_outgoing <-
      unlist(map(gateway_nodes_outgoing, length))
    return(gateway_nodes_outgoing)
  }

# Returns the number of all split gateways of a certain gateway type
number_split_gateways <-
  function (xml_internal_doc,
            gateway_type,
            signavio = FALSE) {
    split_gateways_nodes <-
      split_gateways(xml_internal_doc, gateway_type, signavio)
    return(length(split_gateways_nodes))
  }

# Returns a vector with all the ids of split gateway nodes of a certain type
split_gateways <-
  function(xml_internal_doc,
           gateway_type,
           signavio = FALSE) {
    # Calculate the number of outgoing flows for each gateway
    if (!signavio) {
      gateway_nodes <- getNodeSet(xml_internal_doc, gateway_type)
    } else {
      gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   gateway_type,
                   namespace(xml_internal_doc))
    }
    gateway_nodes_outgoing <- xmlApply(gateway_nodes,
                                       xmlElementsByTagName,
                                       name = "outgoing",
                                       recursive = FALSE)
    gateway_nodes_outgoing <-
      unlist(map(gateway_nodes_outgoing, length))
    
    #get the indices of all gateways which have more than one outgoing flow
    split_indices <- which(gateway_nodes_outgoing > 1)
    
    #get the id of all gateway nodes with the indices of the previous line
    id_gateway_nodes <-
      unlist(xmlApply(gateway_nodes, xmlGetAttr, name = "id"))
    split_gateway_nodes <- id_gateway_nodes[split_indices]
  }

# Returns a vector with all the ids of split gateway nodes
all_split_gateways <- function(xml_internal_doc, signavio = FALSE) {
  if (!signavio) {
    splits <-
      split_gateways(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //bpmn:parallelGateway |
        //bpmn:inclusiveGateway | //bpmn:eventBasedGateway | //bpmn:complexGateway |
        //exclusiveGateway | //parallelGateway |
        //inclusiveGateway | //eventBasedGateway | //complexGateway"
      )
  } else {
    splits <-
      split_gateways(
        xml_internal_doc,
        "//xmlns:exclusiveGateway | //xmlns:parallelGateway |
        //xmlns:inclusiveGateway | //xmlns:eventBasedGateway | //xmlns:complexGateway",
        TRUE
      )
  }
  return(splits)
}

# Returns the number of all join gateways of a certain gateway type
number_join_gateways <-
  function (xml_internal_doc,
            gateway_type,
            signavio = FALSE) {
    join_gateways_nodes <-
      join_gateways(xml_internal_doc, gateway_type, signavio)
    return(length(join_gateways_nodes))
  }

# Returns a vector with all the ids of join gateway nodes of a certain type
join_gateways <-
  function(xml_internal_doc,
           gateway_type,
           signavio = FALSE) {
    # Calculate the number of incoming flows for each gateway
    if (!signavio) {
      gateway_nodes <- getNodeSet(xml_internal_doc, gateway_type)
    } else {
      gateway_nodes <-
        getNodeSet(xml_internal_doc,
                   gateway_type,
                   namespace(xml_internal_doc))
    }
    gateway_nodes_incoming <- xmlApply(gateway_nodes,
                                       xmlElementsByTagName,
                                       name = "incoming",
                                       recursive = FALSE)
    gateway_nodes_incoming <-
      unlist(map(gateway_nodes_incoming, length))
    
    #get the indices of all gateways which have more than one incoming flow
    join_indices <- which(gateway_nodes_incoming > 1)
    
    #get the id of all gateway nodes with the indices of the previous line
    id_gateway_nodes <-
      unlist(xmlApply(gateway_nodes, xmlGetAttr, name = "id"))
    join_gateway_nodes <- id_gateway_nodes[join_indices]
  }

# Returns a vector with all the ids of join gateway nodes
all_join_gateways <- function(xml_internal_doc, signavio  = FALSE) {
  if (!signavio) {
    joins <-
      join_gateways(
        xml_internal_doc,
        "//bpmn:exclusiveGateway | //bpmn:parallelGateway |
        //bpmn:inclusiveGateway | //bpmn:eventBasedGateway | //bpmn:complexGateway |
        //exclusiveGateway | //parallelGateway |
        //inclusiveGateway | //eventBasedGateway | //complexGateway"
      )
  } else {
    joins <-
      join_gateways(
        xml_internal_doc,
        "//xmlns:exclusiveGateway | //xmlns:parallelGateway |
        //xmlns:inclusiveGateway | //xmlns:eventBasedGateway | //xmlns:complexGateway",
        signavio
      )
  }
  return(joins)
}
