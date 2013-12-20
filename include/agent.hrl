-record(agent_state,
        {name, 
         callback, 
         int_state, 
         acl_queue, 
         dict,
         registered,
         process_queue}).
