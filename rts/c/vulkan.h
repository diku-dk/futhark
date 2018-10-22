/* The simple Vulkan runtime framework used by Futhark. */

#include <vulkan/vulkan.h>
#include <stdlib.h>

#define VULKAN_SUCCEED(e) vulkan_succeed(e, #e, __FILE__, __LINE__)

struct vulkan_config {
  uint32_t api_version;
  int debugging;
  int logging;
  int preferred_device_index;
  int preferred_device_queue_index;

  const char* dump_program_to;
  const char* load_program_from;

  size_t default_group_size;
  size_t default_num_groups;
  size_t default_tile_size;
  size_t default_threshold;
  size_t transpose_block_dim;

  int default_group_size_changed;
  int default_tile_size_changed;

  int num_sizes;
  const char **size_names;
  size_t *size_values;
  const char **size_classes;
};

void vulkan_config_init(struct vulkan_config *cfg,
                        int num_sizes,
                        const char *size_names[],
                        size_t *size_values,
                        const char *size_classes[]) {
  cfg->api_version = VK_MAKE_VERSION(1, 1, 0);
  cfg->debugging = 0;
  cfg->logging = 0;
  cfg->preferred_device_index = -1;
  cfg->preferred_device_queue_index = -1;
  cfg->dump_program_to = NULL;
  cfg->load_program_from = NULL;

  cfg->default_group_size = 256;
  cfg->default_num_groups = 128;
  cfg->default_tile_size = 32;
  cfg->default_threshold = 32*1024;
  cfg->transpose_block_dim = 16;

  cfg->default_group_size_changed = 0;
  cfg->default_tile_size_changed = 0;

  cfg->num_sizes = num_sizes;
  cfg->size_names = size_names;
  cfg->size_values = size_values;
  cfg->size_classes = size_classes;
}

struct vulkan_context {
  VkInstance instance;
  VkPhysicalDevice physical_device;
  VkDevice device;
  VkQueue queue;
  uint32_t queue_family_index;
  VkCommandPool command_pool;
  VkCommandBuffer command_buffer;
  VkShaderModule shader_module;
  VkDescriptorPool descriptor_pool;
  VkDescriptorSet *descriptor_sets;
  VkDescriptorSetLayout *descriptor_set_layouts;
  VkPipelineLayout *pipeline_layouts;
  uint32_t entry_point_count;

  struct vulkan_config cfg;

  struct free_list free_list;

  size_t max_group_size;
  size_t max_num_groups;
  size_t max_tile_size;
  size_t max_threshold;

  size_t lockstep_width;
};

// Error codes taken directly from specification
// https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkResult.html
static const char* vulkan_error_string(unsigned int err)
{
    switch (err) {
        case VK_SUCCESS:                            return "Command successfully completed";
        case VK_NOT_READY:                          return "A fence or query has not yet completed";
        case VK_TIMEOUT:                            return "A wait operation has not completed in the specified time";
        case VK_EVENT_SET:                          return "An event is signaled";
        case VK_EVENT_RESET:                        return "An event is unsignaled";
        case VK_INCOMPLETE:                         return "A return array was too small for the result";
        case VK_ERROR_OUT_OF_DEVICE_MEMORY :        return "A device memory allocation has failed";
        case VK_ERROR_INITIALIZATION_FAILED:        return "Initialization of an object could not be completed for implementation-specific reasons";
        case VK_ERROR_DEVICE_LOST:                  return "The logical or physical device has been lost";
        case VK_ERROR_MEMORY_MAP_FAILED:            return "Mapping of a memory object has failed";
        case VK_ERROR_LAYER_NOT_PRESENT:            return "A requested layer is not present or could not be loaded";
        case VK_ERROR_EXTENSION_NOT_PRESENT:        return "A requested extension is not supported";
        case VK_ERROR_FEATURE_NOT_PRESENT:          return "A requested feature is not supported";
        case VK_ERROR_INCOMPATIBLE_DRIVER:          return "The requested version of Vulkan is not supported by the driver or is otherwise incompatible for implementation-specific reasons";
        case VK_ERROR_TOO_MANY_OBJECTS:             return "Too many objects of the type have already been created";
        case VK_ERROR_FORMAT_NOT_SUPPORTED:         return "A requested format is not supported on this device";
        case VK_ERROR_FRAGMENTED_POOL:              return "A pool allocation has failed due to fragmentation of the pool’s memory";
        case VK_ERROR_OUT_OF_POOL_MEMORY:           return "A pool memory allocation has failed";
        case VK_ERROR_INVALID_EXTERNAL_HANDLE:      return "An external handle is not a valid handle of the specified type";
        case VK_ERROR_SURFACE_LOST_KHR:             return "A surface is no longer available";
        case VK_ERROR_NATIVE_WINDOW_IN_USE_KHR:     return "The requested window is already in use by Vulkan or another API in a manner which prevents it from being used again";
        case VK_SUBOPTIMAL_KHR:                     return "A swapchain no longer matches the surface properties exactly, but can still be used to present to the surface successfully";
        case VK_ERROR_OUT_OF_DATE_KHR:              return "A surface has changed in such a way that it is no longer compatible with the swapchain, and further presentation requests using the swapchain will fail";
        case VK_ERROR_INCOMPATIBLE_DISPLAY_KHR:     return "The display used by a swapchain does not use the same presentable image layout, or is incompatible in a way that prevents sharing an image";
        case VK_ERROR_INVALID_SHADER_NV:            return "One or more shaders failed to compile or link";
        case VK_ERROR_FRAGMENTATION_EXT:            return "A descriptor pool creation has failed due to fragmentation";
        default:                                    return "Unknown";
    }
}

static void vulkan_succeed(unsigned int ret,
                           const char *call,
                           const char *file,
                           int line) {
  if (ret != VK_SUCCESS) {
    panic(-1, "%s:%d: Vulkan call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, vulkan_error_string(ret));
  }
}

static int get_suitable_physical_device(const struct vulkan_config *cfg, VkPhysicalDevice *const devices,
                               uint32_t device_count, VkPhysicalDevice* device) {
  for (uint32_t i = 0; i < device_count; ++i)
  {
    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(devices[i], &properties);

    if (properties.apiVersion < cfg->api_version)
      continue;

    *device = devices[i];
    return 1;
  }
  return 0;
}

static VkPhysicalDevice get_preferred_physical_device(const struct vulkan_config *cfg, VkInstance instance) {
  uint32_t device_count = 0;
  VULKAN_SUCCEED(vkEnumeratePhysicalDevices(instance, &device_count, 0));

  VkPhysicalDevice *const devices = (VkPhysicalDevice *)malloc(sizeof(VkPhysicalDevice) * device_count);

  VULKAN_SUCCEED(vkEnumeratePhysicalDevices(instance, &device_count, devices));

  VkPhysicalDevice device;
  if (cfg->preferred_device_index >= 0) {
    if(cfg->preferred_device_index >= device_count)
      panic(1, "Specified device index not found.\n");

    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(devices[cfg->preferred_device_index], &properties);

    if (properties.apiVersion < cfg->api_version)
      panic(1, "Device at specified index does not support required Vulkan API.");
    
    device = devices[cfg->preferred_device_index];
  }
  else if(!get_suitable_physical_device(cfg, devices, device_count, &device)) {
    panic(1, "Could not find acceptable Vulkan device.\n");
  }

  free(devices);
  return device;
}

static uint32_t get_suitable_queue_family(const struct vulkan_config *cfg,
                                          VkQueueFamilyProperties *const queue_props,
                                          uint32_t queue_prop_count,
                                          uint32_t *queue_family_index) {
  for (uint32_t i = 0; i < queue_prop_count; ++i)
  {
      const VkQueueFlags masked_flags = (~(VK_QUEUE_TRANSFER_BIT | VK_QUEUE_SPARSE_BINDING_BIT) &
                                        queue_props[i].queueFlags);

      if (VK_QUEUE_COMPUTE_BIT & masked_flags)
      {
          *queue_family_index = i;
          return 1;
      }
  }
  
  return 0;
}

static uint32_t get_preferred_queue_family(const struct vulkan_config *cfg, VkPhysicalDevice physical_device) {
  uint32_t queue_family_props_count = 0;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_props_count, 0);

  VkQueueFamilyProperties *const queue_family_props = (VkQueueFamilyProperties *)malloc(
    sizeof(VkQueueFamilyProperties) * queue_family_props_count);

  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_props_count, queue_family_props);

  uint32_t queue_family_index;
  if (cfg->preferred_device_queue_index >= 0) {
    if(cfg->preferred_device_queue_index >= queue_family_props_count)
      panic(1, "Specified queue family index not found.\n");
      
      const VkQueueFlags masked_flags = (~(VK_QUEUE_TRANSFER_BIT | VK_QUEUE_SPARSE_BINDING_BIT) &
                                        queue_family_props[cfg->preferred_device_queue_index].queueFlags);

    if (VK_QUEUE_COMPUTE_BIT & masked_flags)
      panic(1, "Queue family does not support compute shaders.");
    
    queue_family_index = cfg->preferred_device_queue_index;
  }
  else if(!get_suitable_queue_family(cfg, queue_family_props, queue_family_props_count, &queue_family_index)) {
    panic(1, "Could not find acceptable device queue.\n");
  }
  
  free(queue_family_props);
  return queue_family_index;
}

static void init_sizes(struct vulkan_context *ctx) {
  VkPhysicalDeviceProperties properties;
  vkGetPhysicalDeviceProperties(ctx->physical_device, &properties);

  size_t max_group_size = properties.limits.maxComputeWorkGroupSize[0];

  size_t max_tile_size = sqrt(max_group_size);

  if (max_group_size < ctx->cfg.default_group_size) {
    if (ctx->cfg.default_group_size_changed) {
      fprintf(stderr, "Note: Device limits default group size to %zu (down from %zu).\n",
              max_group_size, ctx->cfg.default_group_size);
    }
    ctx->cfg.default_group_size = max_group_size;
  }

  if (max_tile_size < ctx->cfg.default_tile_size) {
    if (ctx->cfg.default_tile_size_changed) {
      fprintf(stderr, "Note: Device limits default tile size to %zu (down from %zu).\n",
              max_tile_size, ctx->cfg.default_tile_size);
    }
    ctx->cfg.default_tile_size = max_tile_size;
  }

  ctx->max_group_size = max_group_size;
  ctx->max_tile_size = max_tile_size; // No limit.
  ctx->max_threshold = ctx->max_num_groups = 0; // No limit.

  // Now we go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    const char *size_class = ctx->cfg.size_classes[i];
    size_t *size_value = &ctx->cfg.size_values[i];
    const char* size_name = ctx->cfg.size_names[i];
    size_t max_value, default_value;
    if (strstr(size_class, "group_size") == size_class) {
      max_value = max_group_size;
      default_value = ctx->cfg.default_group_size;
    } else if (strstr(size_class, "num_groups") == size_class) {
      max_value = max_group_size; // Futhark assumes this constraint.
      default_value = ctx->cfg.default_num_groups;
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = sqrt(max_group_size);
      default_value = ctx->cfg.default_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      max_value = 0; // No limit.
      default_value = ctx->cfg.default_threshold;
    } else {
      panic(1, "Unknown size class for size '%s': %s\n", size_name, size_class);
    }
    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %d (down from %d)\n",
              size_name, (int)max_value, (int)*size_value);
      *size_value = max_value;
    }
  }
}

static void setup_vulkan(struct vulkan_context *ctx,
                         const uint32_t entry_point_count,
                         const uint32_t shader[],
                         const uint32_t shader_size) {
  ctx->lockstep_width = 1;

  free_list_init(&ctx->free_list);

  if (ctx->cfg.dump_program_to != NULL) {
    FILE *f = fopen(ctx->cfg.dump_program_to, "wb");
    assert(f != NULL);
    fwrite(shader, sizeof(uint32_t), shader_size / sizeof(uint32_t), f);
    fclose(f);
  }

  VkApplicationInfo application_info;
  application_info.sType              = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  application_info.pNext              = 0;
  application_info.pApplicationName   = "";
  application_info.applicationVersion = 0;
  application_info.pEngineName        = "Futhark";
  application_info.engineVersion      = 0;
  application_info.apiVersion         = ctx->cfg.api_version;

  VkInstanceCreateInfo instance_create_info;
  instance_create_info.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  instance_create_info.pNext                   = 0;
  instance_create_info.flags                   = 0;
  instance_create_info.pApplicationInfo        = &application_info;
  instance_create_info.enabledLayerCount       = 0;
  instance_create_info.ppEnabledLayerNames     = 0;
  instance_create_info.enabledExtensionCount   = 0;
  instance_create_info.ppEnabledExtensionNames = 0;

  VULKAN_SUCCEED(vkCreateInstance(&instance_create_info, 0, &ctx->instance));

  ctx->physical_device = get_preferred_physical_device(&ctx->cfg, ctx->instance);

  init_sizes(ctx);

  ctx->queue_family_index = get_preferred_queue_family(&ctx->cfg, ctx->physical_device);

  const float queue_prio = 1.0f;
  VkDeviceQueueCreateInfo device_queue_create_info;
  device_queue_create_info.sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  device_queue_create_info.pNext            = 0;
  device_queue_create_info.flags            = 0;
  device_queue_create_info.queueFamilyIndex = ctx->queue_family_index;
  device_queue_create_info.queueCount       = 1;
  device_queue_create_info.pQueuePriorities = &queue_prio;
  
  const char *device_extension_names[] = { "VK_KHR_8bit_storage" };

  VkPhysicalDevice8BitStorageFeaturesKHR storage_8bit_features;
  storage_8bit_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR;
  storage_8bit_features.pNext                             = 0;
  storage_8bit_features.storageBuffer8BitAccess           = true;

  VkPhysicalDeviceFeatures device_features;
  device_features.shaderInt16   = true;
  device_features.shaderInt64   = true;
  device_features.shaderFloat64 = true;

  VkDeviceCreateInfo device_create_info;
  device_create_info.sType                   = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  device_create_info.pNext                   = &storage_8bit_features;
  device_create_info.flags                   = 0;
  device_create_info.queueCreateInfoCount    = 1;
  device_create_info.pQueueCreateInfos       = &device_queue_create_info;
  device_create_info.enabledLayerCount       = 0;
  device_create_info.ppEnabledLayerNames     = 0;
  device_create_info.enabledExtensionCount   = sizeof(device_extension_names) / sizeof(char*);
  device_create_info.ppEnabledExtensionNames = device_extension_names;
  device_create_info.pEnabledFeatures        = &device_features;

  VULKAN_SUCCEED(vkCreateDevice(ctx->physical_device, &device_create_info, 0, &ctx->device));

  vkGetDeviceQueue(ctx->device, ctx->queue_family_index, 0, &ctx->queue);

  VkCommandPoolCreateInfo command_pool_create_info;
  command_pool_create_info.sType            = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  command_pool_create_info.pNext            = 0;
  command_pool_create_info.flags            = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
  command_pool_create_info.queueFamilyIndex = ctx->queue_family_index;

  VULKAN_SUCCEED(vkCreateCommandPool(ctx->device, &command_pool_create_info, 0, &ctx->command_pool));

  VkCommandBufferAllocateInfo command_buffer_allocate_info;
  command_buffer_allocate_info.sType              = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  command_buffer_allocate_info.pNext              = 0;
  command_buffer_allocate_info.commandPool        = ctx->command_pool;
  command_buffer_allocate_info.level              = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  command_buffer_allocate_info.commandBufferCount = 1;

  VULKAN_SUCCEED(vkAllocateCommandBuffers(ctx->device, &command_buffer_allocate_info, &ctx->command_buffer));

  VkShaderModuleCreateInfo shader_module_create_info;
  shader_module_create_info.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  shader_module_create_info.pNext    = 0;
  shader_module_create_info.flags    = 0;
  shader_module_create_info.codeSize = shader_size;
  shader_module_create_info.pCode    = shader;

  VULKAN_SUCCEED(vkCreateShaderModule(ctx->device, &shader_module_create_info, 0, &ctx->shader_module));

  ctx->entry_point_count = entry_point_count;
  ctx->descriptor_set_layouts = (VkDescriptorSetLayout*)malloc(entry_point_count * sizeof(VkDescriptorSetLayout));
  ctx->descriptor_sets = (VkDescriptorSet*)malloc(entry_point_count * sizeof(VkDescriptorSet));
  ctx->pipeline_layouts = (VkPipelineLayout*)malloc(entry_point_count * sizeof(VkPipelineLayoutCreateInfo));
}

VkResult vulkan_init_descriptor_pool(struct vulkan_context *ctx, uint32_t total_desc_count) {
  VkDescriptorPoolSize descriptor_pool_size;
  descriptor_pool_size.type = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
  descriptor_pool_size.descriptorCount = total_desc_count;

  VkDescriptorPoolCreateInfo descriptor_pool_create_info;
  descriptor_pool_create_info.sType         = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  descriptor_pool_create_info.pNext         = 0;
  descriptor_pool_create_info.flags         = 0;
  descriptor_pool_create_info.maxSets       = ctx->entry_point_count;
  descriptor_pool_create_info.poolSizeCount = 1;
  descriptor_pool_create_info.pPoolSizes    = &descriptor_pool_size;

  VkResult error = vkCreateDescriptorPool(ctx->device, &descriptor_pool_create_info, 0, &ctx->descriptor_pool);

  if (error != VK_SUCCESS)
    return error;

  VkDescriptorSetAllocateInfo descriptor_set_allocate_info;
  descriptor_set_allocate_info.sType              = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  descriptor_set_allocate_info.pNext              = 0;
  descriptor_set_allocate_info.descriptorPool     = ctx->descriptor_pool;
  descriptor_set_allocate_info.descriptorSetCount = ctx->entry_point_count;
  descriptor_set_allocate_info.pSetLayouts        = ctx->descriptor_set_layouts;

  return vkAllocateDescriptorSets(ctx->device, &descriptor_set_allocate_info, ctx->descriptor_sets);
}

VkResult vulkan_alloc(struct vulkan_context *ctx, size_t min_size, const char *tag, struct vk_buffer_mem_pair *mem_out) {
  assert(min_size >= 0);

  VkBufferCreateInfo buffer_create_info;
  buffer_create_info.sType                 = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  buffer_create_info.pNext                 = 0;
  buffer_create_info.flags                 = 0;
  buffer_create_info.size                  = min_size;
  buffer_create_info.usage                 = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT;
  buffer_create_info.sharingMode           = VK_SHARING_MODE_EXCLUSIVE;
  buffer_create_info.queueFamilyIndexCount = 1;
  buffer_create_info.pQueueFamilyIndices   = &ctx->queue_family_index;

  VkResult error = vkCreateBuffer(ctx->device, &buffer_create_info, 0, &mem_out->buffer);

  // If buffer creation fails we cannot recover
  if (error != VK_SUCCESS) {
    return error;
  }
  
  VkMemoryRequirements mem_req;
  vkGetBufferMemoryRequirements(ctx->device, mem_out->buffer, &mem_req);

  mem_out->size = mem_req.size;

  size_t size;
  struct vk_buffer_mem_pair mem;
  if (free_list_find(&ctx->free_list, tag, &size, &mem) == 0) {
    // Successfully found a free block.  Is it big enough?
    // FIXME: See opencl_alloc in opencl.h
    if (size >= mem_req.size) {
      vkDestroyBuffer(ctx->device, mem_out->buffer, 0);
      *mem_out = mem;
      return VK_SUCCESS;
    }

    // Not just right - free it.
    vkFreeMemory(ctx->device, mem.memory, 0);
    vkDestroyBuffer(ctx->device, mem.buffer, 0);
  }

  // Find suitable heap
  
  VkPhysicalDeviceMemoryProperties properties;
  vkGetPhysicalDeviceMemoryProperties(ctx->physical_device, &properties);

  uint32_t mem_type_i = VK_MAX_MEMORY_TYPES;
  for (uint32_t k = 0; k < properties.memoryTypeCount; k++)
  {
      if ((VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT & properties.memoryTypes[k].propertyFlags) &&
          (VK_MEMORY_PROPERTY_HOST_COHERENT_BIT & properties.memoryTypes[k].propertyFlags) &&
          (mem_req.size < properties.memoryHeaps[properties.memoryTypes[k].heapIndex].size))
      {
          mem_type_i = k;
          break;
      }
  }

  // We have to allocate a new block from the driver.  If the
  // allocation does not succeed, then we might be in an out-of-memory
  // situation.  We now start freeing things from the free list until
  // we think we have freed enough that the allocation will succeed.
  // Since we don't know how far the allocation is from fitting, we
  // have to check after every deallocation.  This might be pretty
  // expensive.  Let's hope that this case is hit rarely.

  VkMemoryAllocateInfo mem_alloc_info;
  mem_alloc_info.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  mem_alloc_info.pNext           = 0;
  mem_alloc_info.allocationSize  = mem_req.size;
  mem_alloc_info.memoryTypeIndex = mem_type_i;

  error = vkAllocateMemory(ctx->device, &mem_alloc_info, 0, &mem_out->memory);

  while (error != VK_SUCCESS) {
    if (free_list_first(&ctx->free_list, &mem) != 0) {
      break;
    }
    vkFreeMemory(ctx->device, mem.memory, 0);
    vkDestroyBuffer(ctx->device, mem.buffer, 0);
    error = vkAllocateMemory(ctx->device, &mem_alloc_info, 0, &mem_out->memory);
  }

  return vkBindBufferMemory(ctx->device, mem_out->buffer, mem_out->memory, 0);
}

void vulkan_free(struct vulkan_context *ctx, struct vk_buffer_mem_pair mem, const char *tag) {
  struct vk_buffer_mem_pair existing_mem;

  // If there is already a block with this tag, then remove it.
  size_t size;
  if (free_list_find(&ctx->free_list, tag, &size, &existing_mem) == 0) {
    vkFreeMemory(ctx->device, existing_mem.memory, 0);
    vkDestroyBuffer(ctx->device, existing_mem.buffer, 0);
  }

  free_list_insert(&ctx->free_list, mem.size, mem, tag);
}

void vulkan_free_all(struct vulkan_context *ctx) {
  struct vk_buffer_mem_pair mem;
  free_list_pack(&ctx->free_list);
  while (free_list_first(&ctx->free_list, &mem) == 0) {
    vkFreeMemory(ctx->device, mem.memory, 0);
    vkDestroyBuffer(ctx->device, mem.buffer, 0);
  }
}

static void vulkan_cleanup(struct vulkan_context *ctx) {
  vulkan_free_all(ctx);
  for (int i = 0; i < ctx->entry_point_count; ++i) {
    vkDestroyPipelineLayout(ctx->device, ctx->pipeline_layouts[i], 0);
  }
  free(ctx->pipeline_layouts);
  for (int i = 0; i < ctx->entry_point_count; ++i) {
    vkDestroyDescriptorSetLayout(ctx->device, ctx->descriptor_set_layouts[i], 0);
  }
  free(ctx->descriptor_set_layouts);
  free(ctx->descriptor_sets);
  vkDestroyDescriptorPool(ctx->device, ctx->descriptor_pool, 0);
  vkResetCommandBuffer(ctx->command_buffer, 0);
  vkFreeCommandBuffers(ctx->device, ctx->command_pool, 1, &ctx->command_buffer);
  vkDestroyCommandPool(ctx->device, ctx->command_pool, 0);
  vkDestroyShaderModule(ctx->device, ctx->shader_module, 0);
  vkDestroyDevice(ctx->device, 0);
  vkDestroyInstance(ctx->instance, 0);
}